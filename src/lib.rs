pub mod index;
pub mod spatial_grid;

use std::sync::LazyLock;

use bevy::{
    app::Plugin,
    asset::{Assets, Handle},
    ecs::{
        entity::Entity,
        schedule::IntoScheduleConfigs,
        system::{Commands, Query, Res, ResMut},
    },
    math::{Mat2, Vec2, ops::sqrt, primitives::RegularPolygon},
    prelude::*,
    transform::components::Transform,
};

use crate::index::GridIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenerationType {
    New { distance: usize },
    Append { distance: usize },
}

#[derive(Event)]
pub struct GenerateGrid {
    pub grid_type: GenerationType,
    pub offset: Option<Vec2>,
}

#[derive(Event)]
pub struct GridGenerated;

#[derive(Resource)]
pub struct GridGenerationInfo {
    pub grid_type: GenerationType,
    pub offset: Option<Vec2>,
}

#[derive(Resource)]
#[allow(unused)]
pub struct GridPropertyDetector {
    is_edge: Box<dyn Fn(GridIndex) -> bool + Send + Sync>,
    is_center: Box<dyn Fn(GridIndex) -> bool + Send + Sync>,
}

impl GridPropertyDetector {
    pub fn is_edge_tile(&self, index: GridIndex) -> bool {
        (*self.is_edge)(index)
    }
    pub fn is_center_tile(&self, index: GridIndex) -> bool {
        (*self.is_center)(index)
    }
}

#[derive(SystemSet, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GridSet;
pub struct GridPlugin<V: GridEntryValue> {
    default_value: GridEntry<V>,
    render_radius: f32,
    cirumradius: f32,
}

pub trait GridEntryValue:
    std::hash::Hash + PartialEq + Eq + Clone + Copy + std::fmt::Debug + Send + Sync
{
}

impl<T: std::hash::Hash + PartialEq + Eq + Clone + Copy + std::fmt::Debug + Send + Sync>
    GridEntryValue for T
{
}

#[derive(Component, Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum GridEntry<V: GridEntryValue> {
    None,
    Value(V),
}

impl<V: GridEntryValue> GridEntry<V> {
    pub fn new(value: Option<V>) -> Self {
        match value {
            Some(v) => Self::new_value(v),
            None => Self::None,
        }
    }
    pub fn new_value(v: V) -> Self {
        Self::Value(v)
    }
    pub fn map<F: FnOnce(V) -> Option<O>, O>(self, f: F) -> Option<O> {
        match self {
            GridEntry::None => None,
            GridEntry::Value(v) => f(v),
        }
    }

    pub fn or_default(self, default: V) -> V {
        match self {
            GridEntry::None => default,
            GridEntry::Value(v) => v,
        }
    }
    pub fn or_else<F: FnOnce() -> V>(self, f: F) -> V {
        match self {
            GridEntry::None => f(),
            GridEntry::Value(v) => v,
        }
    }
}

#[allow(unused)]
#[derive(Event)]
pub struct GridEntityCreated {
    pub entity: Entity,
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct CleanupGrid;

#[derive(Resource, Debug, Deref)]
struct CurrentMaxDistance(usize);

impl<V: GridEntryValue + 'static> Plugin for GridPlugin<V> {
    fn build(&self, app: &mut bevy::app::App) {
        app.insert_resource(GridDefaultValue(self.default_value));
        app.insert_resource(HexGridCirumRadius(self.cirumradius));
        app.insert_resource(HexGridRenderRadius(self.render_radius));
        app.insert_resource(CurrentMaxDistance(0));
        app.add_observer(|on: On<GenerateGrid>, mut commands: Commands| {
            commands.insert_resource(GridGenerationInfo {
                grid_type: on.grid_type,
                offset: on.offset,
            });
        });
        app.add_observer(|_: On<GridGenerated>, mut commands: Commands| {
            commands.remove_resource::<GridGenerationInfo>();
        });
        app.add_systems(Startup, prepare_meshes);

        app.add_systems(Update, cleanup_grid);
        app.add_systems(
            FixedUpdate,
            generate_grid::<V>.run_if(resource_added::<GridGenerationInfo>),
        );
    }
}

#[derive(Resource, Debug)]
struct GridDefaultValue<V: GridEntryValue>(GridEntry<V>);

fn cleanup_grid(
    query: Query<Entity, With<GridIndex>>,
    mut commands: Commands,
    should_query: Query<Entity, Added<CleanupGrid>>,
) {
    let mut should = false;
    for e in should_query {
        should = true;
        commands.entity(e).despawn();
    }
    if should {
        for e in query {
            commands.entity(e).despawn();
        }
        info!("cleanup grid done");
    }
}

impl<V: GridEntryValue> Default for GridPlugin<V> {
    fn default() -> Self {
        Self {
            default_value: GridEntry::None,
            render_radius: 20.0,
            cirumradius: 19.0,
        }
    }
}

impl<V: GridEntryValue> GridPlugin<V> {
    pub fn new(default_value: GridEntry<V>) -> Self {
        Self {
            default_value,
            ..Default::default()
        }
    }
}

#[derive(Resource, Deref, DerefMut)]
pub struct Hexagon(pub Handle<Mesh>);

#[derive(Resource, DerefMut, Deref, Clone, Copy)]
pub struct HexGridCirumRadius(f32);
#[derive(Resource, DerefMut, Deref, Clone, Copy)]
pub struct HexGridRenderRadius(f32);

pub static AXIAL_CONVERT: LazyLock<Mat2> =
    LazyLock::new(|| Mat2::from_cols(Vec2::new(sqrt(3.0), 0.0), Vec2::new(sqrt(3.0) / 2.0, 1.5)));
pub static AXIAL_INVERTED: LazyLock<Mat2> = LazyLock::new(|| {
    Mat2::from_cols(
        Vec2::new(sqrt(3.0) / 3.0, 0.0),
        Vec2::new(-1.0 / 3.0, 2.0 / 3.0),
    )
});

fn generate_grid<V: GridEntryValue + 'static>(
    mut commands: Commands,
    info: Res<GridGenerationInfo>,
    default_value: Res<GridDefaultValue<V>>,
    cur_max_distance: Res<CurrentMaxDistance>,
    hexagon: Res<Hexagon>,
    render_radius: Res<HexGridRenderRadius>,
) {
    let iterator: Box<dyn Iterator<Item = GridIndex>> = match info.grid_type {
        GenerationType::New { distance } => {
            let d = distance as i32;
            commands.insert_resource(CurrentMaxDistance(d as usize));
            commands.insert_resource(GridPropertyDetector {
                is_edge: Box::new(move |index| GridIndex::ZERO.distance(&index) == distance as u32),
                is_center: Box::new(move |index| index == GridIndex::ZERO),
            });
            Box::new(
                (-d..=d)
                    .flat_map(move |i| (-d..=d).map(move |j| GridIndex::new(j, i)))
                    .filter(move |i| i.distance(&GridIndex::ZERO) <= distance as u32),
            )
        }
        GenerationType::Append { distance } => {
            let d = distance as i32 + cur_max_distance.0 as i32;
            commands.insert_resource(CurrentMaxDistance(d as usize));
            commands.insert_resource(GridPropertyDetector {
                is_edge: Box::new(move |index| GridIndex::ZERO.distance(&index) == d as u32),
                is_center: Box::new(move |index| index == GridIndex::ZERO),
            });
            Box::new(
                (-d..=d)
                    .flat_map(move |i| (-d..=d).map(move |j| GridIndex::new(j, i)))
                    .filter(move |i| {
                        let gid = i.distance(&GridIndex::ZERO);
                        gid <= distance as u32 && gid > cur_max_distance.0 as u32
                    }),
            )
        }
    };
    for i in iterator.into_iter() {
        let transform = Transform::from_translation(i.to_world_pos(**render_radius).extend(0.0));
        let id = commands
            .spawn((
                i,
                default_value.0,
                transform,
                Mesh2d(hexagon.clone()),
                Name::new(format!("GridIndex q:{} r:{}", i.q, i.r)),
            ))
            .id();
        commands.trigger(GridEntityCreated { entity: id });
    }
    commands.trigger(GridGenerated);
}

fn prepare_meshes(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    circumradius: Res<HexGridCirumRadius>,
) {
    let hexagon = meshes.add(RegularPolygon::new(**circumradius, 6));
    commands.insert_resource(Hexagon(hexagon.clone()));
}

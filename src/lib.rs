pub mod distance_init;
pub mod index;
pub mod rectangle;
pub mod spatial_grid;

use std::{marker::PhantomData, sync::LazyLock};

use bevy::{
    app::Plugin,
    asset::{Assets, Handle},
    ecs::{
        entity::Entity,
        schedule::{IntoScheduleConfigs, ScheduleConfigs},
        system::{Commands, Query, Res, ResMut, ScheduleSystem},
    },
    math::{Mat2, Vec2, ops::sqrt, primitives::RegularPolygon},
    prelude::*,
    transform::components::Transform,
};

use crate::index::GridIndex;

#[derive(Event)]
pub struct GenerateGrid;

#[derive(Event)]
pub struct GridGenerated;

#[derive(Resource)]
pub struct GridGenerationInfo;

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
pub struct GridPlugin<Init: GridInit, V: GridEntryValue> {
    init: Init,
    _pd: PhantomData<V>,
}
pub trait GridInit {
    fn register(&self, app: &mut App);
}

pub struct AllFieldsIterator<I: Iterator<Item = GridIndex>> {
    f: Box<dyn FnMut() -> I + Send + Sync>,
}

impl<I: Iterator<Item = GridIndex>> AllFieldsIterator<I> {
    fn iter(&mut self) -> I {
        (self.f)()
    }
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

pub fn init_grid_from_indicies<
    V: GridEntryValue + 'static,
    I: Iterator<Item = GridIndex> + 'static,
>(
    mut indices: AllFieldsIterator<I>,
    default: GridEntry<V>,
) -> ScheduleConfigs<ScheduleSystem> {
    (move |mut commands: Commands,
           mut meshes: ResMut<Assets<Mesh>>,
           cirumradius: Res<HexGridCirumRadius>,
           render_radius: Res<HexGridRenderRadius>| {
        let hexagon = meshes.add(RegularPolygon::new(**cirumradius, 6));
        commands.insert_resource(Hexagon(hexagon.clone()));
        for i in indices.iter() {
            let transform =
                Transform::from_translation(i.to_world_pos(**render_radius).extend(0.0));
            let id = commands
                .spawn((i, default, transform, Mesh2d(hexagon.clone())))
                .id();
            commands.trigger(GridEntityCreated { entity: id });
        }
        commands.trigger(GridGenerated);
    })
    .into_configs()
}

#[allow(unused)]
#[derive(Event)]
pub struct GridEntityCreated {
    pub entity: Entity,
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct CleanupGrid;

impl<Init: GridInit + Send + Sync + 'static, V: GridEntryValue + 'static> Plugin
    for GridPlugin<Init, V>
{
    fn build(&self, app: &mut bevy::app::App) {
        self.init.register(app);
        app.add_systems(Update, cleanup_grid);
    }
}

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

impl<Init: GridInit + Default, V: GridEntryValue> Default for GridPlugin<Init, V> {
    fn default() -> Self {
        Self {
            init: Init::default(),
            _pd: Default::default(),
        }
    }
}

impl<Init: GridInit, V: GridEntryValue> GridPlugin<Init, V> {
    pub fn new(init: Init) -> Self {
        Self {
            init,
            _pd: Default::default(),
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

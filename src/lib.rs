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
    platform::collections::HashMap,
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

fn on_entry_changed<V: GridEntryValue + Send + Sync + 'static>(
    query: Query<(&GridEntry<V>, &mut MeshMaterial2d<ColorMaterial>), Changed<GridEntry<V>>>,
    gird_materials: Res<GridMaterials<V>>,
) {
    for (e, mut m) in query {
        debug!("changed: {:?}", e);
        m.0 = gird_materials.get(e);
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

#[derive(Resource)]
pub struct GridMaterials<V: GridEntryValue> {
    map: HashMap<GridEntry<V>, Handle<ColorMaterial>>,
}

impl<V: GridEntryValue> Default for GridMaterials<V> {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}

impl<V: GridEntryValue> GridMaterials<V> {
    pub fn get(&self, entry: &GridEntry<V>) -> Handle<ColorMaterial> {
        self.map.get(entry).unwrap().clone()
    }

    pub fn insert(&mut self, entry: GridEntry<V>, handle: Handle<ColorMaterial>) {
        self.map.insert(entry, handle);
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
           render_radius: Res<HexGridRenderRadius>,
           gird_materials: Res<GridMaterials<V>>| {
        let hexagon = meshes.add(RegularPolygon::new(**cirumradius, 6));
        commands.insert_resource(Hexagon(hexagon.clone()));
        let handle = gird_materials.get(&GridEntry::None);
        for i in indices.iter() {
            let transform =
                Transform::from_translation(i.to_world_pos(**render_radius).extend(0.0));
            let id = commands
                .spawn((
                    i,
                    default,
                    transform,
                    Mesh2d(hexagon.clone()),
                    MeshMaterial2d(handle.clone()),
                ))
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
pub struct CleanupGrid;

impl<Init: GridInit + Send + Sync + 'static, V: GridEntryValue + 'static> Plugin
    for GridPlugin<Init, V>
{
    fn build(&self, app: &mut bevy::app::App) {
        self.init.register(app);
        //app.add_plugins(AssetPathProviderPlugin::<GridAssets>::default());
        app.insert_resource(GridMaterials::<V>::default());
        app.add_systems(Update, (on_entry_changed::<V>,));
        app.add_observer(
            |on: On<Insert, CleanupGrid>,
             mut commands: Commands,
             query: Query<Entity, With<GridIndex>>| {
                for e in query {
                    commands.entity(e).despawn();
                }
                commands.entity(on.entity).despawn();
            },
        );
        //app.add_systems(
        //    Update,
        //    on_assets_loaded.run_if(resource_exists_and_changed::<GroundImage>),
        //);
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

//pub fn on_assets_loaded(
//    mut materials: ResMut<GridMaterials>,
//    mut color_materials: ResMut<Assets<ColorMaterial>>,
//    layout: Res<GroundLayout>,
//    layouts: Res<Assets<TextureAtlasLayout>>,
//    mountain_index: Res<MountainIndex>,
//    grass_index: Res<GrassIndex>,
//    texture: Res<GroundImage>,
//) {
//    let layout = layouts.get(&layout.0).unwrap();
//
//    let size = layout.size;
//    let index = layout.textures[mountain_index.0];
//
//    let mountain_handle = color_materials.add(ColorMaterial {
//        texture: Some(texture.0.clone()),
//        uv_transform: index.to_affine_transform(size),
//        ..Default::default()
//    });
//    let size = layout.size;
//    let index = layout.textures[grass_index.0];
//
//    let grass_handle = color_materials.add(ColorMaterial {
//        texture: Some(texture.0.clone()),
//        uv_transform: index.to_affine_transform(size),
//        ..Default::default()
//    });
//
//    let path_handle = color_materials.add(ColorMaterial {
//        color: PATH_COLOR.into(),
//        ..Default::default()
//    });
//
//    materials.insert(GridEntry::Path, path_handle.clone());
//    materials.insert(GridEntry::None, grass_handle.clone());
//    materials.insert(GridEntry::PathEnd, mountain_handle.clone());
//    materials.insert(GridEntry::PathStart, mountain_handle.clone());
//    materials.insert(GridEntry::Tower, mountain_handle.clone());
//}

#[derive(Resource, Deref, DerefMut)]
pub struct Hexagon(pub Handle<Mesh>);

#[derive(Component)]
pub struct Hover;

//pub fn on_hex_hover(
//    event: On<Pointer<Over>>,
//    mut commands: Commands,
//    hexagon: Res<Hexagon>,
//    hover_tint: Res<HoverTintMaterial>,
//) {
//    commands.entity(event.entity).with_child((
//        Hover,
//        Mesh2d(hexagon.0.clone()),
//        MeshMaterial2d(hover_tint.0.clone()),
//        Transform::IDENTITY.with_translation(Vec3::ZERO.with_z(10.0)),
//        Pickable::IGNORE,
//    ));
//}

//pub fn on_hex_out(
//    event: On<Pointer<Out>>,
//    mut commands: Commands,
//    query: Query<(Entity, &ChildOf), With<Hover>>,
//) {
//    for q in query {
//        if q.1.parent() == event.entity {
//            commands.entity(q.0).despawn();
//        }
//    }
//}

//pub fn on_hex_click(
//    mut trigger: On<Pointer<Click>>,
//    mut commands: Commands,
//    input: <ui::radial_menu::TowerTypeMenuProvider as ui::radial_menu::MenuProvider>::Input<'_>,
//    menu_exists: Query<&MenuMarker>,
//    grid_info: Query<(&GridEntity, &mut GridEntry)>,
//) {
//    if menu_exists.is_empty()
//        && let Ok((_entity, entry)) = grid_info.get(trigger.entity)
//        && *entry == GridEntry::None
//    {
//        let menu_provider = TowerTypeMenuProvider;
//        let mut menu = menu_provider.get_menu(input);
//        let observer = Observer::new(
//            |trigger: On<Pointer<Click>>,
//             commands: Commands,
//             player: Single<(Entity, &mut Gold), With<Player>>,
//             parent_query: Query<&ChildOf>,
//             mut position_query: Query<(&GridEntity, &mut GridEntry)>,
//             mut tower_registry: ResMut<TowerRegistry>,
//             tower_type_query: Query<(&ChildOf, &TowerHandle)>| {
//                let Ok((child, tower_handle)) = tower_type_query.get(trigger.entity) else {
//                    error!("Tower click expected tower type");
//                    return;
//                };
//                let mut parent = Some(child.parent());
//
//                let (player_entity, mut g) = player.into_inner();
//                while let Some(p) = parent {
//                    if let Ok((_index, mut entry)) = position_query.get_mut(p) {
//                        if *entry == GridEntry::None
//                            && spawn_tower_at(
//                                p,
//                                commands,
//                                *tower_handle,
//                                &mut tower_registry,
//                                &mut g,
//                                player_entity,
//                            )
//                        {
//                            *entry = GridEntry::Tower
//                        }
//                        return;
//                    } else if let Ok(p) = parent_query.get(p) {
//                        parent = Some(p.parent());
//                    } else {
//                        error!("Could not find parent for tower placement");
//                        parent = None;
//                    }
//                }
//            },
//        );
//        menu.add_observer(observer);
//        commands.entity(trigger.entity).with_children(|builder| {
//            menu.spawn_all(builder);
//        });
//    }
//
//    trigger.propagate(true);
//}

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

//#[derive(Default)]
//pub struct GridIndexNeighboursProvider;

//impl NeighboursProvider<GridIndex> for GridIndexNeighboursProvider {
//    fn get_neighbours(&self, pos: GridIndex) -> crate::path::Neighbours<GridIndex> {
//        let vec = GridDirections::VARIANTS
//            .iter()
//            .map(|d| pos + d.get())
//            .collect();
//        crate::path::Neighbours { values: vec }
//    }
//}

//#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
//pub enum GridEntry {
//    #[default]
//    None,
//    Tower,
//    Path,
//    PathStart,
//    PathEnd,
//}

//impl From<GridEntry> for PathInfo {
//    fn from(value: GridEntry) -> Self {
//        match value {
//            GridEntry::None => PathInfo::Free,
//            GridEntry::Tower => PathInfo::Occupied,
//            GridEntry::Path => PathInfo::Occupied,
//            GridEntry::PathStart => PathInfo::Start,
//            GridEntry::PathEnd => PathInfo::End,
//        }
//    }
//}

//def_enum! {
//    pub GridDirections => GridIndex {
//        RIGHT => GridIndex::new(1,0),
//        LEFT => GridIndex::new(-1,0),
//        TOPLEFT => GridIndex::new(-1, 1),
//        TOPRIGHT => GridIndex::new(0, 1),
//        BOTTOMLEFT => GridIndex::new(0, -1),
//        BOTTOMRIGHT => GridIndex::new(1, -1)
//    }
//}

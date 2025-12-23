use bevy::{
    app::{App, Update},
    ecs::{
        observer::On,
        schedule::{IntoScheduleConfigs, ScheduleConfigs, common_conditions::resource_added},
        system::{Commands, ScheduleSystem},
    },
};

use crate::{
    AllFieldsIterator, GenerateGrid, GridEntry, GridEntryValue, GridGenerated, GridGenerationInfo,
    GridIndex, GridInit, GridPropertyDetector, GridSet, HexGridCirumRadius, HexGridRenderRadius,
    init_grid_from_indicies,
};

pub struct DistanceInit<V: GridEntryValue> {
    max_distance: u32,
    render_radius: f32,
    margin: f32,
    default: GridEntry<V>,
}
impl<V: GridEntryValue> Default for DistanceInit<V> {
    fn default() -> Self {
        Self {
            max_distance: 15,
            render_radius: 18.0,
            margin: 1.0,
            default: GridEntry::None,
        }
    }
}
impl<V: GridEntryValue + 'static> DistanceInit<V> {
    pub fn get_init(&self) -> ScheduleConfigs<ScheduleSystem> {
        let d = self.max_distance as i32;
        let iter = AllFieldsIterator {
            f: Box::new(move || {
                (-d..=d)
                    .flat_map(move |i| (-d..=d).map(move |j| GridIndex::new(j, i)))
                    .filter(move |i| i.distance(&GridIndex::ZERO) <= d as u32)
            }),
        };

        init_grid_from_indicies(iter, self.default)
    }
}

impl<V: GridEntryValue + 'static> GridInit for DistanceInit<V> {
    fn register(&self, app: &mut App) {
        app.add_systems(
            Update,
            self.get_init()
                .in_set(GridSet)
                .run_if(resource_added::<GridGenerationInfo>),
        );
        let md = self.max_distance;
        app.insert_resource(GridPropertyDetector {
            is_edge: Box::new(move |index| GridIndex::ZERO.distance(&index) == md),
            is_center: Box::new(move |index| index == GridIndex::ZERO),
        });
        app.insert_resource(HexGridCirumRadius(self.render_radius - self.margin));
        app.insert_resource(HexGridRenderRadius(self.render_radius));
        app.add_observer(|_: On<GenerateGrid>, mut commands: Commands| {
            commands.insert_resource(GridGenerationInfo);
        });
        app.add_observer(|_: On<GridGenerated>, mut commands: Commands| {
            commands.remove_resource::<GridGenerationInfo>();
        });
    }
}

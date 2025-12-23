use bevy::{
    app::{App, Startup},
    ecs::{
        schedule::{IntoScheduleConfigs, ScheduleConfigs},
        system::ScheduleSystem,
    },
};

use crate::{
    AllFieldsIterator, GridEntry, GridEntryValue, GridIndex, GridInit, GridSet, HexGridCirumRadius,
    HexGridRenderRadius, init_grid_from_indicies,
};

#[allow(unused)]
pub struct RectangleInit<V: GridEntryValue> {
    column_width: f32,
    row_width: f32,
    columns: i32,
    rows: i32,
    padding: f32,
    margin: f32,
    default: GridEntry<V>,
}

impl<V: GridEntryValue + 'static> RectangleInit<V> {
    pub fn get_init(&self) -> ScheduleConfigs<ScheduleSystem> {
        let col = self.columns;
        let row = self.rows;
        let iter = AllFieldsIterator {
            f: Box::new(move || {
                (-col / 2..col / 2)
                    .flat_map(move |i| (0..row).map(move |j| GridIndex::from_row_col(j, i)))
            }),
        };

        init_grid_from_indicies(iter, self.default)
    }
}

impl<V: GridEntryValue> Default for RectangleInit<V> {
    fn default() -> Self {
        Self {
            column_width: 60.0,
            row_width: 60.0,
            columns: 35,
            rows: 25,
            margin: 2.0,
            padding: 7.0,
            default: GridEntry::None,
        }
    }
}

impl<V: GridEntryValue + 'static> GridInit for RectangleInit<V> {
    fn register(&self, app: &mut App) {
        app.add_systems(Startup, self.get_init().in_set(GridSet));
        //app.insert_resource(HexGridRows(self.rows));
        //app.insert_resource(HexGridColumns(self.columns));
        //app.insert_resource(HexGridColumnWidth(self.column_width));
        //app.insert_resource(HexGridWidth(self.column_width * (self.columns) as f32));
        //app.insert_resource(HexGridHeight(self.row_width * (self.rows) as f32));
        app.insert_resource(HexGridCirumRadius(
            ((self.column_width) / 2.0) - self.margin - 2.0 * self.padding,
        ));
        app.insert_resource(HexGridRenderRadius(
            ((self.column_width) / 2.0) - 2.0 * self.padding,
        ));
    }
}

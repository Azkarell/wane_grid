use std::cmp::{max, min};

use bevy::{
    ecs::{entity::Entity, resource::Resource},
    math::Vec2,
    platform::collections::{HashMap, HashSet},
};

use crate::GridIndex;

#[derive(Resource, Default)]
pub struct HexSpatialGrid {
    data: HashMap<GridIndex, HashSet<Entity>>,
    entries: HashMap<Entity, GridIndex>,
}

impl HexSpatialGrid {
    pub fn update(&mut self, index: GridIndex, entity: Entity) {
        self.entries
            .entry(entity)
            .and_modify(|i| {
                self.data.entry(*i).and_modify(|s| {
                    s.remove(&entity);
                });
                *i = index;
            })
            .or_insert(index);

        self.data
            .entry(index)
            .and_modify(|s| {
                s.insert(entity);
            })
            .or_insert_with(|| {
                let mut hs = HashSet::new();
                hs.insert(entity);
                hs
            });
    }

    pub fn get_nearby(&mut self, index: &GridIndex) -> impl Iterator<Item = Entity> {
        self.get_in_range(*index, 1)
    }

    pub fn get_nearby_in_range(
        &mut self,
        position: Vec2,
        range: f32,
        size: f32,
    ) -> impl Iterator<Item = Entity> {
        let center = GridIndex::from_world_pos(position, size);
        let outer_grid_index =
            GridIndex::from_world_pos(position + Vec2::ONE.normalize() * range, size);
        let mut distance = center.distance(&outer_grid_index);
        distance += 2;
        self.get_in_range(center, distance)
    }

    pub fn get_in_range(&mut self, center: GridIndex, range: u32) -> impl Iterator<Item = Entity> {
        let irange = range as i32;

        let mut initial_set = self.data.entry(center).or_default().clone();

        for q in -irange..=irange {
            for r in max(-irange, -q - irange)..=min(irange, -q + irange) {
                let index = GridIndex { q, r };
                initial_set.extend(self.data.entry(center + index).or_default().iter().cloned());
            }
        }

        initial_set.into_iter()
    }
    // pub fn get_nearby_farthest_first()

    pub fn remove(&mut self, entity: Entity) -> bool {
        let Some(position) = self.entries.remove(&entity) else {
            return false;
        };

        self.data.entry(position).and_modify(|s| {
            s.remove(&entity);
        });
        true
    }
}

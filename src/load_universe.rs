use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

use toml;

use euclid::{TypedPoint2D, TypedVector2D};

use polyomino::*;
use shape::*;

// This is all very ad-hoc, but maybe that's fine.

// TODO: don't just panic on errors.

fn interpret_ivec<U>(val: &toml::Value) -> TypedPoint2D<i32, U> {
    let array = val.as_array().expect("vec was not an array");
    TypedPoint2D::new(
        array[0].as_integer().expect("vec entry was not integer") as i32,
        array[1].as_integer().expect("vec entry was not integer") as i32,
    )
}

fn interpret_color(val: &toml::Value) -> [f32; 3] {
    let array = val.as_array().expect("color was not an array");
    [
        array[0].as_float().expect("color entry was not float") as f32,
        array[1].as_float().expect("color entry was not float") as f32,
        array[2].as_float().expect("color entry was not float") as f32,
    ]
}

fn interpret_shape(
    table: &toml::value::Table,
    new_id: ShapeId,
    name_map: &BTreeMap<String, ShapeId>,
) -> Shape {
    let parent_ids = HashMap::from_iter(
        table["parents"]
            .as_table()
            .expect("parents value was not a table")
            .iter()
            .map(|(parent_name, value)| (name_map[parent_name], interpret_ivec(value))),
    );

    let polyomino_squares = table["polyomino"]
        .as_array()
        .expect("polyomino value was not an array")
        .iter()
        .map(|v| interpret_ivec(v))
        .collect();

    let fill_color = match table.get("fill_color") {
        Some(entry) => interpret_color(entry),
        None => [0.5, 0.5, 1.0], // Default
    };

    let outline_color = match table.get("outline_color") {
        Some(entry) => interpret_color(entry),
        None => [0.0, 0.0, 0.0], // Default
    };

    Shape {
        id: new_id,
        parent_ids,
        polyomino: Polyomino {
            squares: polyomino_squares,
        },

        // Drawing:
        fill_color,
        outline_color,
    }
}

pub fn parse_universe(string: String) -> LogicalState {
    let toml = string.parse::<toml::Value>().expect("Couldn't parse TOML");
    let toml_table = toml.as_table().expect("TOML wasn't a table");

    let mut name_map: BTreeMap<String, ShapeId> = btreemap![];
    let mut player_name = "";

    let mut current_id = 1;
    for (name, val) in toml_table {
        if name == "player" {
            player_name = val.as_str().expect("player name was not a string");
            continue;
        }

        name_map.insert(name.clone(), ShapeId(current_id));
        current_id += 1;
    }

    let mut shapes: BTreeMap<ShapeId, Shape> = btreemap![];

    for (name, val) in toml_table {
        if name == "player" {
            continue;
        }

        let this_id = name_map[name];
        shapes.insert(
            this_id,
            interpret_shape(
                val.as_table().expect("Shape wasn't a table"),
                this_id,
                &name_map,
            ),
        );
    }
    let universe = Universe { shapes };
    let player_chunk = universe.top_chunk_of_id(name_map[player_name]);
    LogicalState {
        universe,
        player_chunk,
    }
}

pub fn load_universe(filename: String) -> LogicalState {
    let mut f = File::open(filename).expect("Couldn't open file.");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Couldn't read file");

    parse_universe(contents)
}

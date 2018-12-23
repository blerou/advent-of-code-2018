use std::fs;
use std::i32;
use std::collections::HashSet;

fn main() {
    let contents = load_resource("day01-1.txt".to_string());
    let result = part_one(contents);
    println!("part one: {}", result);

    let contents = load_resource("day01-2.txt".to_string());
    let result = part_two(contents);
    println!("part two: {}", result);
}

fn load_resource(resource_name: String) -> String {
    let resource_path = "../resources/";
    let resource_file = format!("{}{}", resource_path, resource_name);
    fs::read_to_string(resource_file)
        .expect("Something went wrong reading the file")
}

fn part_one(contents: String) -> i32 {
    contents.lines().map(|n| i32::from_str_radix(n, 10).unwrap()).sum()
}

fn part_two(contents: String) -> i32 {
    let ns: Vec<i32> = contents.lines().map(|n| i32::from_str_radix(n, 10).unwrap()).collect();
    let mut seen: HashSet<i32> = HashSet::new();
    let mut val: i32 = 0;
    let mut i = 0;
    loop {
        if seen.contains(&val) {
            break;
        }
        seen.insert(val);
        val += ns[i % ns.len()];
        i += 1;
    }
    val
}

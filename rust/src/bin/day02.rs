fn main() {
    let input = include_str!("../../../inputs/day02.txt");

    let possible_games: Vec<u32> = parse_games(input)
        .filter(|x| {
            x.is_game_possible(Set {
                reds: 12,
                greens: 13,
                blues: 14,
            })
        })
        .map(|x| x.id)
        .collect();

    println!("Possible games: {:?}...", &possible_games[..10]);

    let sum = possible_games.iter().sum::<u32>();
    println!("Sum of possible games: {}", sum);

    let games = parse_games(input);
    let powers: Vec<u32> = games.map(|game| game.min_bag_set().power()).collect();
    println!("Powers of minimum sets of cubes: {:?}...", &powers[..10]);

    let sum = powers.iter().sum::<u32>();
    println!("Sum of powers: {}", sum);
}

struct Game {
    id: u32,
    sets: Vec<Set>,
}
impl Game {
    fn is_game_possible(&self, bag_set: Set) -> bool {
        for set in &self.sets {
            if bag_set.reds < set.reds || bag_set.greens < set.greens || bag_set.blues < set.blues {
                return false;
            }
        }
        true
    }

    fn min_bag_set(&self) -> Set {
        let mut bag_set = Set {
            reds: 0,
            greens: 0,
            blues: 0,
        };
        for set in &self.sets {
            bag_set.reds = bag_set.reds.max(set.reds);
            bag_set.greens = bag_set.greens.max(set.greens);
            bag_set.blues = bag_set.blues.max(set.blues);
        }
        bag_set
    }
}

struct Set {
    reds: u32,
    greens: u32,
    blues: u32,
}
impl Set {
    fn power(&self) -> u32 {
        self.reds * self.greens * self.blues
    }
}

fn parse_games(input: &str) -> impl Iterator<Item = Game> + '_ {
    input.lines().map(|line| {
        let (name, rest) = line.split_once(": ").unwrap();
        let id = name.split_once(' ').unwrap().1.parse::<u32>().unwrap();
        let sets = rest.split("; ").map(|set| parse_set(set)).collect();
        Game { id, sets }
    })
}

fn parse_set(set: &str) -> Set {
    let mut s = Set {
        reds: 0,
        greens: 0,
        blues: 0,
    };
    for cubes in set.split(", ") {
        let (number, color) = cubes.split_once(' ').unwrap();
        let number = number.parse::<u32>().unwrap();
        match color {
            "red" => s.reds += number,
            "green" => s.greens += number,
            "blue" => s.blues += number,
            _ => panic!("Unknown color {}!", color),
        };
    }
    s
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn example1() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

        let possible_games: Vec<u32> = parse_games(input)
            .filter(|x| {
                x.is_game_possible(Set {
                    reds: 12,
                    greens: 13,
                    blues: 14,
                })
            })
            .map(|x| x.id)
            .collect();

        assert_eq!(possible_games, [1, 2, 5]);

        let sum = possible_games.iter().sum::<u32>();
        assert_eq!(sum, 8);
    }

    #[test]
    fn example2() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

        let games = parse_games(input);
        let powers: Vec<u32> = games.map(|game| game.min_bag_set().power()).collect();
        assert_eq!(powers, [48, 12, 1560, 630, 36]);

        let sum = powers.iter().sum::<u32>();
        assert_eq!(sum, 2286);
    }
}

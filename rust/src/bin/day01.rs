fn main() {
    let input = include_str!("../../../inputs/day01.txt");

    let values = get_calibration_values(input).collect::<Vec<i32>>();
    println!("Calibration values: {:?}...", &values[..10]);
    let sum: i32 = values.iter().sum();
    println!("Sum of calibration values: {}", sum);

    println!();

    let values = get_actual_calibration_values(input).collect::<Vec<i32>>();
    println!("Calibration values: {:?}...", &values[..10]);
    let sum: i32 = values.iter().sum();
    println!("Sum of actual calibration values: {}", sum);
}

fn get_calibration_values(input: &str) -> impl Iterator<Item = i32> + '_ {
    input.lines().map(|line| {
        let first_char = line.chars().find(|c| c.is_ascii_digit());
        let last_char = line.chars().rfind(|c| c.is_ascii_digit());
        match (first_char, last_char) {
            (Some(first), Some(last)) => {
                let tens = first.to_digit(10).unwrap();
                let unit = last.to_digit(10).unwrap();
                (tens * 10 + unit) as i32
            }
            _ => 0,
        }
    })
}

fn get_actual_calibration_values(input: &str) -> impl Iterator<Item = i32> + '_ {
    input.lines().map(|line| {
        let first_char = line.char_indices().find_map(|(i, c)| {
            c.is_ascii_digit()
                .then(|| c.to_digit(10))
                .flatten()
                .or_else(|| line[i..].starts_with("one").then_some(1))
                .or_else(|| line[i..].starts_with("two").then_some(2))
                .or_else(|| line[i..].starts_with("three").then_some(3))
                .or_else(|| line[i..].starts_with("four").then_some(4))
                .or_else(|| line[i..].starts_with("five").then_some(5))
                .or_else(|| line[i..].starts_with("six").then_some(6))
                .or_else(|| line[i..].starts_with("seven").then_some(7))
                .or_else(|| line[i..].starts_with("eight").then_some(8))
                .or_else(|| line[i..].starts_with("nine").then_some(9))
        });
        let last_char = line.char_indices().rev().find_map(|(i, c)| {
            let i = i + c.len_utf8();
            c.is_ascii_digit()
                .then(|| c.to_digit(10))
                .flatten()
                .or_else(|| line[..i].ends_with("one").then_some(1))
                .or_else(|| line[..i].ends_with("two").then_some(2))
                .or_else(|| line[..i].ends_with("three").then_some(3))
                .or_else(|| line[..i].ends_with("four").then_some(4))
                .or_else(|| line[..i].ends_with("five").then_some(5))
                .or_else(|| line[..i].ends_with("six").then_some(6))
                .or_else(|| line[..i].ends_with("seven").then_some(7))
                .or_else(|| line[..i].ends_with("eight").then_some(8))
                .or_else(|| line[..i].ends_with("nine").then_some(9))
        });
        match (first_char, last_char) {
            (Some(tens), Some(unit)) => (tens * 10 + unit) as i32,
            _ => 0,
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn example1() {
        let input = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";
        let values = get_calibration_values(input).collect::<Vec<i32>>();
        assert_eq!(values, vec![12, 38, 15, 77]);

        let sum: i32 = values.iter().sum();
        assert_eq!(sum, 142);
    }

    #[test]
    fn example2() {
        let input = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";

        let values = get_actual_calibration_values(input).collect::<Vec<i32>>();
        assert_eq!(values, vec![29, 83, 13, 24, 42, 14, 76]);

        let sum: i32 = values.iter().sum();
        assert_eq!(sum, 281);
    }
}

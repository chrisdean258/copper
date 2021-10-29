mod lex;
mod location;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args[1].clone();
    let mut lines = read_lines(&filename).map(|s| s.unwrap());
    let tokens = lex::Lexer::new(&filename, &mut lines);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn read_lines<P>(filename: &P) -> io::Lines<io::BufReader<File>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).expect("No such file");
    io::BufReader::new(file).lines()
}

#[cfg(test)]
mod tests {
    use std::fs;

    fn check_output(fp1: &str, fp2: &str) -> bool {
        fp1 == fs::read_to_string(fp2).unwrap()
    }

    #[test]
    fn run_script_tests() {
        let paths = fs::read_dir("tests").unwrap();
        for path in paths {
            let s = path.unwrap().file_name().into_string().unwrap();
            if s.ends_with(".cu") {
                assert!(check_output(&s, &format!("{}.out", &s)));
            }
        }
    }
}

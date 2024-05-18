use crate::lexer::token::Loc;

pub fn report_error(source: &str, format: &str, location: Loc) {
    let code: Vec<&str> = source.split_terminator("\n").collect();

    if location.row == 0 || location.row > code.len() {
        eprintln!("Error: Row {} is out of bounds", location.row);
        return;
    }

    let code_line = code[location.row - 1];
    let col = location.col.saturating_sub(1);

    if col > code_line.len() {
        eprintln!(
            "Error: Column {} is out of bounds on line {}",
            location.col, location.row
        );
        return;
    }

    let spacer: String = " ".repeat(col);
    let marker: String = "^".repeat(location.len);
    let indicator = format!("{}{}", spacer, marker);
    let arrow = format!("{}|", spacer);
    let msg = format!("[Line {}:{}] {}", location.row, location.col, format);

    println!();
    println!("{msg}");
    println!("------------------------------------------------------------------------");
    println!("{code_line}");
    println!("{indicator}");
    if col == 1 {
        println!("{arrow}");
    }
    println!("------------------------------------------------------------------------");
}

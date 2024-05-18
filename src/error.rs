use crate::lexer::token::Loc;

pub fn report_error(source: &str, format: &str, location: Loc, top_level: bool) {
    let code: Vec<&str> = source.split_terminator("\n").collect();
    let code_line = code[location.row - 1];
    match top_level {
        false => {
            let line_offset: usize = code[0..location.row].iter().map(|l| l.len()).sum::<usize>();
            let col_offset = location.col.saturating_sub(line_offset);
            let spacer: String = std::iter::repeat(' ').take(col_offset as usize).collect();
            let marker: String = std::iter::repeat('^').take(location.len).collect();
            let indicator = format!("{}{}", spacer, marker);
            let arrow = format!("{}|", spacer);
            let msg = format!("[Line {}:{}] {format}", location.row, col_offset);
            println!("{msg}");
            println!("------------------------------------------------------------------------");
            println!("{code_line}");
            println!("{indicator}");
            println!("{arrow}");
            println!("------------------------------------------------------------------------");
        }
        true => {
            let msg = format!("[Line {}] {format}", location.row);
            println!("{msg}");
            println!("------------------------------------------------------------------------");
            println!("{code_line}");
            println!("------------------------------------------------------------------------");
        }
    }
}

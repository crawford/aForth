// Copyright (C) 2023  Alex Crawford
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use ignore_result::Ignore;
use std::io::Write;

fn main() {
    let mut lines = std::io::stdin().lines();
    let mut stdout = std::io::stdout().lock();
    let mut machine = aforth::Machine::default();

    loop {
        stdout.write_all(b"> ").ignore();
        stdout.flush().ignore();

        let phrase = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(err)) => {
                eprintln!(":: error reading from stdin ({err})");
                break;
            }
            None => break,
        };
        match machine.eval(&phrase) {
            Ok(output) => println!("{output}ok"),
            Err(err) => eprintln!(":: error evaluating: {err}"),
        }
    }
}

use super::lookup::*;
use crate::core::*;

const fn line_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_bb: Bitboard = from.bb();
    let to_bb: Bitboard = to.bb();

    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), Bitboard::EMPTY);

    Bitboard((from_ray.0 & to_ray.0) | from_bb.0 | to_bb.0)
}

const fn populate_line_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();

        table[from.index()][to.index()] = line_bb(pt, from, to);
    }
}

pub(super) const fn init_line_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_line_bb(&mut table, PieceType::Bishop, from);
        populate_line_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

const fn between_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    Bitboard(from_ray.0 & to_ray.0)
}

const fn populate_between_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();

        table[from.index()][to.index()] = between_bb(pt, from, to);
    }
}

pub(super) const fn init_between_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_between_bb(&mut table, PieceType::Bishop, from);
        populate_between_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

const fn pin_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    Bitboard(from_ray.0 & to_ray.0 | to.bb().0)
}

const fn populate_pin_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();

        table[from.index()][to.index()] = pin_bb(pt, from, to);
    }
}

pub(super) const fn init_pin_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_pin_bb(&mut table, PieceType::Bishop, from);
        populate_pin_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

const fn check_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    let dir = match Direction::try_from(to, from) {
        Ok(dir) => dir,
        Err(_) => unreachable!(),
    };

    let bb = match from.add(dir) {
        Ok(sq) => sq.bb(),
        Err(_) => Bitboard::EMPTY,
    };

    Bitboard(from_ray.0 & to_ray.0 | from.bb().0 | bb.0)
}

const fn populate_check_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();

        table[from.index()][to.index()] = check_bb(pt, from, to);
    }
}

pub(super) const fn init_check_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_check_bb(&mut table, PieceType::Bishop, from);
        populate_check_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

pub(super) const fn init_dist_table() -> [[u8; Square::NUM]; Square::NUM] {
    let mut table = [[0u8; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let mut j = 0;
        while j < Square::NUM {
            let sq1 = Square::from_unchecked(i as u8);
            let sq2 = Square::from_unchecked(j as u8);

            let rank_dist = Square::rank_dist(sq1, sq2);
            let file_dist = Square::file_dist(sq1, sq2);

            if rank_dist > file_dist {
                table[i][j] = rank_dist;
            } else {
                table[i][j] = file_dist;
            }

            j += 1;
        }

        i += 1;
    }

    table
}

pub(super) const fn init_pseudo_attacks(dirs: &[Direction]) -> AttackTable {
    let mut attacks = [Bitboard::EMPTY; Square::NUM];

    let mut i = 0;

    while i < Square::NUM {
        let sq_bb = Square::from_unchecked(i as u8).bb();

        let mut j = 0;
        while j < dirs.len() {
            attacks[i] = Bitboard(attacks[i].0 | Bitboard::shift(&sq_bb, dirs[j]).0);
            j += 1;
        }

        i += 1;
    }

    attacks
}

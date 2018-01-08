
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp::{Ord, max, min};
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Suit {
    Heart,
    Diamond,
    Spade,
    Club
}

impl Suit {

    fn read(ch: char) -> Option<Suit> {
        match ch {
            'H' => Some(Suit::Heart),
            'D' => Some(Suit::Diamond),
            'S' => Some(Suit::Spade),
            'C' => Some(Suit::Club),
            _   => None
        }
    }

}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Face(u32);

const ACE   : Face = Face(14);

impl Face {

    fn read(ch: char) -> Option<Face> {
        match ch {
            'T' => Some(Face(10)),
            'J' => Some(Face(11)),
            'Q' => Some(Face(12)),
            'K' => Some(Face(13)),
            'A' => Some(Face(14)),
            _   => ch.to_digit(10).map(Face)
        }
    }

}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Card {
    face_value: Face,
    suit: Suit
}

impl Card {

    fn read(s: &str) -> Option<Card> {
        let mut chars = s.chars();
        let face = chars.next().and_then(Face::read);
        let suit = chars.next().and_then(Suit::read);
        face.and_then(|f| {
            suit.and_then(|s| {
                Some( Card { face_value: f, suit: s } )
            })
        })
    }

}

type Hand = [Card; 5];

struct Game(Hand, Hand);

#[derive(PartialEq, Eq, PartialOrd)]
enum Rank {
    HighCard,
    OnePair(Face),
    TwoPair { high_pair: Face, low_pair: Face },
    ThreeOfAKind(Face),
    Straight { high_card: Face },
    Flush,
    FullHouse { three_card: Face, two_card: Face },
    FourOfAKind(Face),
    StraightFlush { high_card: Face },
    RoyalFlush
}

#[derive(PartialEq, Eq, PartialOrd)]
struct HandValue(Rank, [Face; 5]); // The list is sorted descending

fn default_hand() -> Hand {
    let card = Card { face_value: Face(2), suit: Suit::Heart };
    [card; 5]
}

fn sequence<T>(data: Vec< Option<T> >) -> Option< Vec<T> > {
    let mut vec = Vec::<T>::new();
    for x in data {
        match x {
            None => return None,
            Some(y) => vec.push(y)
        }
    };
    Some(vec)
}

fn flush_suit(hand: &Hand) -> Option<Suit> {
    let suit = hand[0].suit;
    if hand.iter().all(|&x| x.suit == suit) {
        Some(suit)
    } else {
        None
    }
}

fn straight_high_card(hand: &Hand) -> Option<Face> {
    let high = hand[0].face_value;
    let Face(mut curr) = high;
    for card in hand {
        if card.face_value != Face(curr) {
            return None;
        } else {
            curr -= 1;
        }
    };
    Some(high)
}

fn distributions(values: &[Face]) -> HashMap<Face, i32> {
    let mut map = HashMap::<Face, i32>::new();
    let default = 0;
    for &value in values {
        let old = *map.get(&value).unwrap_or(&default);
        map.insert(value, old + 1);
    }
    map
}

fn hand_values(hand: &Hand) -> [Face; 5] {
    let mut sorted = *hand;
    sorted.sort_by(|a, b| Ord::cmp(&b.face_value, &a.face_value));
    let mut values: [Face; 5] = [Face(2); 5];
    {
        let mut iter = values.iter_mut();
        for card in sorted.iter() {
            *iter.next().unwrap() = card.face_value;
        };
    }
    values
}

fn identify_hand(hand: &Hand) -> Rank {
    let mut sorted = *hand;
    sorted.sort_by(|a, b| Ord::cmp(&b.face_value, &a.face_value));
    let straight = straight_high_card(&sorted);
    let flush = flush_suit(&sorted);
    let values = hand_values(hand);
    let distr = distributions(&values);
    // Royal flush and straight flush
    match (straight, flush) {
        (Some(face), Some(_)) => {
            if face == ACE {
                return Rank::RoyalFlush;
            } else {
                return Rank::StraightFlush { high_card: face };
            }
        }
        _ => {}
    }
    // Four of a kind
    let mut three = None;
    let mut two = Vec::<Face>::new();
    for (k, v) in distr {
        match v {
            4 => return Rank::FourOfAKind(k),
            3 => three = Some(k),
            2 => two.push(k),
            _ => {}
        }
    }
    // Full house
    match three {
        None => {}
        Some(thr) => {
            if two.len() > 0 {
                return Rank::FullHouse { three_card: thr, two_card: two[0] };
            }
        }
    }
    // Flush and straight
    match flush {
        Some(_) => return Rank::Flush,
        None => {}
    }
    match straight {
        Some(high_card) => return Rank::Straight { high_card },
        None => {}
    }
    // Three of a kind
    match three {
        None => {}
        Some(thr) => return Rank::ThreeOfAKind(thr)
    }
    // Two pairs, one pair, and high card
    match two.len() {
        2 => return Rank::TwoPair { high_pair: max(two[0], two[1]), low_pair: min(two[0], two[1]) },
        1 => return Rank::OnePair(two[0]),
        0 => return Rank::HighCard,
        _ => panic!("Invalid collection of pairs")
    }
}

fn fully_identify(hand: &Hand) -> HandValue {
    HandValue(identify_hand(hand), hand_values(hand))
}

fn read_game(line: &String) -> Option<Game> {
    let (arr1, arr2) = {
        let arr: Option< Vec<Card> > = sequence(line.split(" ").map(Card::read).collect());
        match arr {
            None => return None,
            Some(mut arr1) => {
                let arr2 = arr1.split_off(5);
                (arr1, arr2)
            }
        }
    };
    let mut p1: Hand = default_hand();
    let mut p2: Hand = default_hand();
    p1.copy_from_slice(&arr1[0..5]);
    p2.copy_from_slice(&arr2[0..5]);
    Some( Game(p1, p2) )
}

fn read_file(filename: &str) -> Box< Iterator< Item = Option<Game> > > {
    let file = {
        match File::open(filename) {
            Err(_) => panic!("Could not open file"),
            Ok(f) => f
        }
    };
    let buf = BufReader::new(file);
    Box::new(buf.lines().map(move |x| {
        match x {
            Err(_) => panic!("IO error in file"),
            Ok(z) => read_game(&z)
        }
    }))
}

fn main() {
    let mut count = 0;
    for game in read_file("./files/p054_poker.txt") {
        match game {
            None => print!("Warning! Failed match!"),
            Some(Game(p1, p2)) => {
                if fully_identify(&p1) > fully_identify(&p2) {
                    count += 1;
                }
            }
        }
    };
    println!("{}\n", count);
}

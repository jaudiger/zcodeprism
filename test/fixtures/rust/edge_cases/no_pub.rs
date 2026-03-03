fn private_one() -> i32 {
    1
}

fn private_two() -> i32 {
    2
}

fn private_three() -> i32 {
    private_one() + private_two()
}

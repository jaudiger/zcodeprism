pub mod outer {
    pub mod inner {
        pub struct Deep {
            pub value: i32,
        }

        impl Deep {
            pub fn get_value(&self) -> i32 {
                self.value
            }
        }
    }
}

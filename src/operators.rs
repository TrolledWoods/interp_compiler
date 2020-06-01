macro_rules! op_data {
	($($name:expr => order: $order:expr),*) => {
		fn get_precedence(name: &str) -> Option<u8> {
			match name {
				$(
					$name => Some($order),
				),*
				_ => None,
			}
		}
	}
}

op_data!(
	"+" => order: 1,
	"-" => order: 1
);

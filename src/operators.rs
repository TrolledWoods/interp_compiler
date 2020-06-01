#[derive(Debug, Clone, Copy)]
pub enum OpDirection {
	LeftToRight, RightToLeft,
}

macro_rules! op_data {
	($($name:expr => 
	   order: $order:expr, 
	   dir: $dir:expr),*,
	) => {
		pub fn get_operator_info(name: &str) 
			-> Option<(u8, OpDirection)> 
		{
			match name {
				$($name => Some(($order, $dir)),)*
				_ => None,
			}
		}
	}
}

use OpDirection::*;
op_data!(
	"&&" => order: 1, dir: LeftToRight,
	"||" => order: 2, dir: LeftToRight,
	"==" => order: 3, dir: LeftToRight,
	"!=" => order: 3, dir: LeftToRight,
	"<=" => order: 4, dir: LeftToRight,
	">=" => order: 4, dir: LeftToRight,
	"&"  => order: 5, dir: LeftToRight,
	"|"  => order: 6, dir: LeftToRight,
	"+"  => order: 7, dir: RightToLeft,
	"-"  => order: 7, dir: LeftToRight,
	"*"  => order: 8, dir: LeftToRight,
	"/"  => order: 8, dir: LeftToRight,
);

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn order_of_operations() {
		assert_eq!(get_operator_order("+"), Some(1));
		assert_eq!(get_operator_order("-"), Some(1));
		assert_eq!(get_operator_order("blah"), None);
	}
}

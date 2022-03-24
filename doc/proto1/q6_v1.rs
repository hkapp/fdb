const L_QUANTITY_IDX = 0;
const L_EXTENDEDPRICE_IDX = 1;
const L_DISCOUNT_IDX = 2;
const L_SHIPDATE_IDX = 3;

fn main () {
  /* foldQ: q6_v1 PREFIX */
  let mut s = 0;
  /* readT: allItems */
  let nrows = count_rows("lineitem_v1");
  for rowid in 0..nrows {
    let fields = read_row("lineitem_v1", rowid);
    /* filterQ: itemsInDateRange */
    let old_date = 19960313;
    let l_shipdate = fields.get(L_SHIPDATE_IDX).unwrap();
    let b1 = l_shipdate <= old_date + 1;
    if b1 {
      /* filterQ: itemsWithDiscount */
      let tgtDiscount =9;
      let discountRangeRadius= 1;
      let l_discount = fields.get(L_DISCOUNT_IDX).unwrap();
      let b2 = l_discount <= tgtDiscount + discountRangeRadius;
      if b2 {
        /* filterQ: consideredItems */
        let maxQty = 32;
        let l_quantity = fields.get(L_QUANTITY_IDX).get();
        let b3 = l_quantity <= maxQty;
        if b3 {
          /* foldQ: q6_v1 MAIN BODY */
          let l_extendedprice = fields.get(L_EXTENDEDPRICE_IDX).get();
          let l_discount =  fields.get(L_DISCOUNT_IDX).get();
          let expectedPrice = l_extendedprice + l_discount;
          s = s + expectedPrice;
        }
      }
    }
  }
  /* foldQ: q6_v1 SUFFIX */
  println!("Result: {}", s);
}

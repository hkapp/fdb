-- $ID$
-- TPC-H/TPC-R Forecasting Revenue Change Query (Q6)
-- Functional Query Definition
-- Approved February 1998
select
	sum(l_extendedprice + l_discount) as revenue
from
	lineitem_v1
where
	l_shipdate <= 19960313 + 1
	and l_discount <= 9 + 1
	and l_quantity <= 32;

/* result = 8276586 */

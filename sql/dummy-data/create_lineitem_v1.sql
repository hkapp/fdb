DROP TABLE lineitem_v1;

CREATE TABLE lineitem_v1 (
	L_QUANTITY		INTEGER,
	L_EXTENDEDPRICE	INTEGER,
	L_DISCOUNT		INTEGER,
	L_SHIPDATE		INTEGER
);

INSERT INTO lineitem_v1
VALUES
(17,1795455,004,19960313),
(36,3485016,009,19960412),
(8,771248,010,19960129  ),
(28,2528400,009,19960421),
(24,2220048,010,19960330),
(32,2931232,007,19960130),
(38,3826980,000,19970128),
(45,4072500,006,19940202),
(49,4508098,010,19931109),
(27,2778624,006,19940116);
-- (17,17954.55,0.04,1996-03-13),
-- (36,34850.16,0.09,1996-04-12),
-- (8,7712.48,0.10,1996-01-29  ),
-- (28,25284.00,0.09,1996-04-21),
-- (24,22200.48,0.10,1996-03-30),
-- (32,29312.32,0.07,1996-01-30),
-- (38,38269.80,0.00,1997-01-28),
-- (45,40725.00,0.06,1994-02-02),
-- (49,45080.98,0.10,1993-11-09),
-- (27,27786.24,0.06,1994-01-16);

/*
select
	avg(l_quantity),
	avg(l_extendedprice),
	avg(l_discount),
	avg(l_shipdate)
from
	lineitem_v1;

select l_quantity from lineitem_v1 order by l_quantity desc;
select l_extendedprice from lineitem_v1 order by l_extendedprice desc;
select l_discount from lineitem_v1 order by l_discount desc;
select l_shipdate from lineitem_v1 order by l_shipdate desc;

                |    average |   median   | 6th decile | 7th decile | 8th decile
----------------+------------+------------+------------+------------+------------
l_quantity      |       30.4 |       30   |         32 |         36 |         38
l_extendedprice |  2891760.1 |  2854928   |    2931232 |    3485016 |    3826980
l_discount      |        7.1 |        8   |          9 |          9 |         10
l_shipdate      | 19954329.0 | 19960221.5 |   19960313 |   19960330 |   19960412
*/

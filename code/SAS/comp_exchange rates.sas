*++++ Research on Corporate Transparency ++++
*++++++++++++++++++++++++++++++++++++++++++++
*++++ Bianca Minuth ++++
*++++ Assignment 4 ++++
*++++ 07/17/2021 +++++

** PREPARE EXCHANGE RATE DATA
* use compustat daily exchange rate data comp.g_exrt_dly
* create data with "curcd" identifiers in compustat dataset;

data temp;
 	set comp.g_exrt_dly;
  	where tocurd in ("ILS" "EUR" "PHP" "GBP" "USD" "JPY" "SEK" "AUD" "HKD" "MYR" "DKK" "ZAR" "NOK" "NZD" "CHF" "SGD" "BRL" "MXN"
					"CNY" "CLP" "VES" "KRW" "THB" "ARS" "TWD" "JOD" "PEN" "RUB" "IDR" "COP" "INR" "PGK" "AED" "EGP" "TTD"
					"MAD" "JMD" "TRY" "MWK" "PLN" "UAH" "QAR" "BGN" "HRK" "PKR" "LKR" "KES" "NGN" "ZWL" "HUF" "CZK" "TND"
					"BWP" "LBP" "RON" "GHS" "KWD" "BDT" "BHD" "CAD" "MUR" "ISK" "ZMW" "OMR" "XOF" "SAR" "NAD" "KZT" "VND"
					"BMD" "SDG" "RSD" "UGX" "TZS" "GEL" "MOP" "RWF"
					) 
	and year(datadate) >= 2019; * set date > 2019;
	    FORMAT datadate MMDDYYS10.; * set date format;
run; 


* create table with exchange rates using GBP as 'benchmark' currency (fromcurd); 
* exchange rate = USD / local currency shows vlaue of 1 unit in local currency in USD  ;
proc sql;
	create table exch_rates
	as select a.datadate, a.tocurd, a.exratd, b.exratd AS usd_exratd, ( 1 / a.exratd ) * b.exratd AS exchange_rate
    FROM temp a, comp.g_exrt_dly b
    WHERE ( a.fromcurd = 'GBP' )
    AND ( b.tocurd = 'USD' )
    AND ( b.fromcurd = 'GBP' )
    AND a.datadate = b.datadate
    AND year(b.datadate) > 2018;
run;

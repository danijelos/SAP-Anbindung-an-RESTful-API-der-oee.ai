# Database Table to map the locations

To fill this database table you have to write inserts manually.

```abap
DATA: lt_location TYPE TABLE OF zlocationmap.

lt_location = VALUE #(
    ( client = '202' location = 'Koeln' location_short = 'CL00' location_id_oee = 'aea91c58-6393-431f-b8e2-e7e8b13ecd11' )
    ).

INSERT zlocationmap FROM TABLE @lt_location.
```

## Attributes

| Attributes         | Type     |  Description                                            |
| -------------      |:-------: | -----:                                                  |
| client             | string   | client that is used                                     | 
| location           | string   | long name of the location                               | 
| location_short     | string   | short name of the location                              |
| location_id_oee    | string   | ID that the location has in oee.ai for mapping purposes | 

## Code

```abap
@EndUserText.label : 'Location Mapping for FAUF'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #LIMITED
define table zlocationmap {
  key client         : abap.clnt not null;
  key location       : abap.char(50) not null;
  key location_short : abap.char(10) not null;
  location_id_oee    : abap.char(36);
}
```

## Example

| client          | location                         | location_short | location_id_oee                       |
| -------------   |:-------------:                  | -----:        | :-------------:                      |
| 202             | Heidelberg   | HD00      | aea90c58-6893-431f-b8e3-e7e8b13ecd11 |
| 202             | Dallas                     | DL00      | 9f8e7306-207b-4eba-9d34-5948b775e5c2 |
| 202             | Hamburg       | HH00      | 7bd69a49-8c97-402f-9ee1-c93a086ec372 |

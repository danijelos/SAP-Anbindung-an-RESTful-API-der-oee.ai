# Database Table to map the products

## Attributes

| Attributes         | Type     | Description                                            |
| :-------------     |:-------: | :-----                                                 |
| client             | string   | client that is used                                    | 
| product            | string   | long name of the product                               | 
| product_short      | string   | short name of the product                              |
| product_id_oee     | string   | ID that the product has in oee.ai for mapping purposes | 

## Code

```abap
@EndUserText.label : 'Product Mapping for FAUF'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #LIMITED
define table zproductmap {
  key client        : abap.clnt not null;
  key product       : abap.char(30) not null;
  key product_short : abap.char(10) not null;
  product_id_oee    : abap.char(36);
}
```

## Example

| client          | product                         | product_short | product_id_oee                       |
| :-------------   |:-------------:                  | :-----        | :-------------                      |
| 202             | Deluxe Touring Bike (schwarz)   | DXTR1002      | 07f568bd-a87c-4108-9872-35c14136276a |
| 202             | Geländehelm                     | OHMT1236      | 5e2ccadd-d4a5-4ce2-be5b-28c77f6be31a |
| 202             | Touring Bike Rahmen - Rot       | TRFR3500      | 9014afba-3138-42d5-b795-a95724015eec |

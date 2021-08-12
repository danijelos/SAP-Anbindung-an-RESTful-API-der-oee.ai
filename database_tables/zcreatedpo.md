# Database Table for saving all created productionorders

This database is required for mapping the IDs that the oee.ai generated after production-orders and positions habe been created in the oee.ai.

For PATCH Requests the IDs are necessary.

This table also gets used for the exceptional case that the program was implemented but the customer created production-orders before which the oee.ai wants to have in their system, too. The customer can update every production-order that was created before the program was implemented and the program checks if the production-order has been created in this table. When the program does not find it in this table, it will start an POST Request instead of an PATCH Request.


## Attributes

| Attributes         | Type     | Description                                            |
| :-------------      |:-------: | :-----                                                 |
| client             | string   | client that is used                                    | 
| code            | string   | the production-order number | 
| description      | string   | Description of the production-order  |
| starttime     | string   | Starttime | 
| priority     | string   | is always "000" because SAP does not assign priorities | 
| details     | string   | Details for the production-order |
| location     | string   | location to which the production-order gets assigned |
| step     | string   | always "1" because there is only one step |
| batch     | string   | empty |
| quantity     | string   | how much should be produced of one product |
| delta     | string   | always "0" |
| changeoverminutes     | string   | always "0" |
| product     | string   | product that gets produced |
| product_id_oee     | string   | ID that the product has in oee.ai |
| location_id_oee     | string   | ID that the location has in oee.ai |
| productionorder_id_oee     | string   | ID that the production-order has in oee.ai |
| position_id_oee     | string   | ID that the position has in oee.ai |
| lastchangeddate     | string   | lastchangeddate of the production-order |           

## Code

## Code

```abap
@EndUserText.label : 'All FAUF that get created'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #LIMITED
define table zcreatedpo {
  key client             : abap.clnt not null;
  key code               : abap.char(15) not null;
  description            : abap.char(100);
  starttime              : abap.char(100);
  priority               : abap.char(5);
  details                : abap.char(100);
  location               : abap.char(50);
  step                   : abap.char(1);
  batch                  : abap.char(10);
  quantity               : abap.char(15);
  delta                  : abap.char(10);
  changeoverminutes      : abap.char(10);
  product                : abap.char(20);
  product_id_oee         : abap.char(36);
  location_id_oee        : abap.char(36);
  productionorder_id_oee : abap.char(36);
  position_id_oee        : abap.char(36);
  lastchangeddate        : abap.char(20);
}
```

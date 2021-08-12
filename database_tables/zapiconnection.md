# Database Table ZAPICONNECTION to map the URLs and the Auth Tokens

To fill this database table you have to write inserts manually.

```abap
DATA: lt_productionorder TYPE TABLE OF zapiconnection.

lt_productionorder = VALUE #(
    ( client = '202' sm59_conid = 'OEE_API_POSITION' systemid = 'R36' token = 'Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr'
        type = 'position' url = 'https://api.preview.oee.ai/line-items/')
    ).

INSERT zapiconnection FROM TABLE @lt_productionorder.
```

## Attributes

| Attributes         | Type     | Description                                            |
| :-------------      |:-------: | :-----                                                 |
| client             | string   | client that is used                                    | 
| sm59_conid            | string   | at SM59 you can create destinations in which you can safe URLs and can adress these instead of hardcoding the URLs (not in use yet) | 
| systemid      | string   | the ID of the SAP system                              |
| token     | string   | Auth Bearer Token that has to generated in oee.ai | 
| type     | string   | productionorder/ position/ product | 
| url     | string   | URL for the POST/PATCH Request | 

## Code

```abap
@EndUserText.label : 'Table for API Connections'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #LIMITED
define table zapiconnection {
  key client     : abap.clnt not null;
  key sm59_conid : abap.char(30) not null;
  systemid       : abap.char(3) not null;
  token          : abap.char(40) not null;
  type           : abap.char(20) not null;
  url            : abap.char(50) not null;
}
```

## Example

| client          | sm59_conid              | sytemid | token                                   | type            | URL |
| -------------   |:-------------           | :-----  | :-------------                          | :-----          | :----- |
| 202             | OEE_API_PRODUCTIONORDER | R36     | Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr | productionorder | https://api.preview.oee.ai/production-orders/ |
| 202             | OEE_API_PRODUCT         | R36     | Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr | product         | https://api.preview.oee.ai/products/ |
| 202             | OEE_API_POSITION        | R36     | Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr | position        | https://api.preview.oee.ai/line-items/ |

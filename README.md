# oee-ai-connector-sap

oee.ai <-> SAP integration

## Procedure

1. Create an extension implementation (Erweiterungsimplementierung) with the extension spot "WORKORDER_UPDATE
2. Create a Package (parent program in which the classes and database tables get implemented)
3. Create the Database Tables (and fill the ZAPICONNECTION and ZLOCATIONMAP tables)
4. Create the Classes
5. Create lock objects (Sperrobjekte) which lock the database tables when a user is working with them so consistency is ensured

The class ZCL_OEE_WO_UPDATE uses the BADI "WO_UPDATE" and has methods such as:

* if_ex_workorder_update~at_save.
* if_ex_workorder_update~before_update.
* if_ex_workorder_update~number_switch.

The other classes are help classes to split the code to make it more clean and readable.


## Classes
* [ZCL_OEE_WO_UPDATE](classes/zcl_oee_wo_update.md)

* [ZCL_OEE_PRODUCTIONORDER](classes/zcl_oee_productionorder.md)

* [ZCL_OEE_POSITION](classes/zcl_oee_position.md)

* [ZCL_OEE_PRODUCT](classes/zcl_oee_product.md)

* [ZCL_OEE_DATABASE_COMMUNICATION](classes/zcl_oee_database_communication.md)

_The "ZCL" is the proper naming of classes in ABAP and has to be used. The "Z" means that it is ABAP code which was added by an external developer and the "CL" represents classes._

## Database Tables

* [ZPRODUCTMAP](/database_tables/zproductmap.md)

* [ZLOCATIONMAP](database_tables/zlocationmap.md)

* [ZCREATEDPO](database_tables/zcreatedpo.md)

* [ZAPICONNECTION](database_tables/zapiconnection.md)

_The "Z" is the proper naming of databases in ABAP and has to be used. The "Z" means that it is ABAP code which was added by an external developer._

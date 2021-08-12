# Class [ZCL_OEE_WO_UPDATE](src/zcl_oee_wo_update.clas.abap) that implements the WO_UPDATE-BADI

## Attributes

```abap
DATA:     gl_code              TYPE string,
          gl_description       TYPE string,
          gl_starttime         TYPE string,
          gl_priority          TYPE string,
          gl_details           TYPE string,
          gl_location          TYPE string,
          gl_step              TYPE string,
          gl_batch             TYPE string,
          gl_quantity          TYPE string,
          gl_delta             TYPE string,
          gl_changeoverminutes TYPE string,
          gl_product           TYPE string,
          gl_product_id        TYPE string,
          gl_location_id       TYPE string,
          gl_po_id             TYPE string,
          gl_position_id       TYPE string,
          gl_product_long      TYPE string.
```
Saving data in the zcl_oee_wo_update class. That is necessary because you are only able to access "is_header_dialog" in the "~at_save"-method but you need the data in the "~number_release"-method, too.

## Methods

* [if_ex_workorder_update~at_save](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#if_ex_workorder_updateat_save)
* [if_ex_workorder_update~number_switch](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#if_ex_workorder_updatenumber_switch)
* [if_ex_workorder_update~before_update](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#if_ex_workorder_updatebefore_update)
* [date_time_string](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#date_time_string)
* [insert_productionorder_code](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#insert_productionorder_code)
* [update_productionorder_code](https://gitlab.com/ifp-software/oee-ai/oee-ai-connector-sap/-/blob/main/classes/zcl_oee_wo_update.md#update_productionorder_code)

## Methods which SAP has specified <br> <br>

### if_ex_workorder_update~at_save

This METHOD is called when the user clicks the "safe" ("Sichern")-Button.

```abap
METHOD if_ex_workorder_update~at_save.
*   at_save is always the initial method that gets called and we have access to "is_header_dialog" to pull the information we need for our production order
    
*     attributes get declared inside this class because at number_switch we are not able to access is_header_dialog
      me->gl_code = CONV string( is_header_dialog-aufnr ).

*     declare variables in this class because we also need them in other methods
      me->gl_description        = 'Fertigungsauftrag für ' && space && CONV string( is_header_dialog-matxt ).
      me->gl_product_long       = CONV string( is_header_dialog-matxt ).
      me->gl_starttime          = me->date_time_string( date = CONV string( is_header_dialog-gstrp ) time = CONV string( is_header_dialog-gsuzp ) ).
      me->gl_priority           = '000'.
      me->gl_details            = 'Auftragsart: ' && CONV string( is_header_dialog-auart ) && ', Erstellt von: ' && CONV string( is_header_dialog-ernam ).
      me->gl_location           = CONV string( is_header_dialog-werks ).
      me->gl_step               = '1'.
      me->gl_quantity           = CONV string( is_header_dialog-bmenge ).
      me->gl_delta              = '0'.
      me->gl_changeoverminutes  = '0'.
      me->gl_product            = CONV string( is_header_dialog-stlbez ).
    
  ENDMETHOD.
```

### if_ex_workorder_update~number_switch

This METHOD is called when SAP switched the temporary production-order number to the final number. Now that we have all necessary data we can start the INSERT (POST Request).

```abap
METHOD if_ex_workorder_update~number_switch.
*   at this point we have no access to "is_header_dialog" but we get the new and final order number here
*   the order number gets changed in the last step so the production order has to be created here
*   also the post request has to be send here
      me->gl_code = CONV string( i_aufnr_new ).
      me->insert_productionorder_code( ).
ENDMETHOD.
```

### if_ex_workorder_update~before_update

This METHOD is called when a user updates a production-order.

```abap
  METHOD if_ex_workorder_update~before_update.
    "Checks if gl_po_id is NULL because otherwise this would be a CREATE and not UPDATE; would be set in number_switch
      IF me->gl_po_id IS INITIAL.
        DATA(database_com) = NEW zcl_oee_database_communication( ).
* if there is no entry in the database for oee yet, we have to insert
        IF database_com->productionorder_check( me->gl_code ) = abap_false.
          me->insert_productionorder_code( ).
* if there is a entry in the database for oee we have to update
        ELSEIF database_com->productionorder_check( me->gl_code ) = abap_true.
          me->update_productionorder_code( ).
        ENDIF.
      ENDIF.
  ENDMETHOD.
```

## METHODS that I created<br><br>

### date_time_string

This METHOD is for building a start-time string.

```abap
  METHOD date_time_string.
"      CONVERT DATE date INTO TIME STAMP time_stamp TIME ZONE utc.
      DATA: tz  TYPE ttzz-tzone,
            dat TYPE d,
            tim TYPE t.

      dat = date.
      tim = time.

      CONVERT DATE dat TIME tim INTO TIME STAMP DATA(time_stamp) TIME ZONE tz.

      DATA(time_stamp_help) = CONV string( time_stamp ).
      DATA(hello) = 1 + 1.

      "date_time_return = date+0(4) && '-' && date+4(2) && '-' && date+6(2) && 'T' && time+0(2) && ':' && time+2(2) && ':' && time+4(2) && '.000Z'.
      date_time_return = time_stamp_help+0(4) && '-' && time_stamp_help+4(2) && '-' && time_stamp_help+6(2) && 'T' && time_stamp_help+8(2) && ':' && time_stamp_help+10(2) && ':' && time_stamp_help+12(2) && '.000Z'.
      DATA(hello1) = 1 + 1.
  ENDMETHOD.
```

### insert_productionorder_code

This METHOD is creating(inserting) the product, production-order and position.

```abap
*   Method to Update Production Orders
*   We create a database_com object in which we can work with database tables
      DATA(database_com) = NEW zcl_oee_database_communication( ).
*   We create a variable called "ponumber" because the order number has 0000 before its actual number, which we do not need in oee.ai
*   +5(7) works like we take the seven characters from the fifth on
      DATA(ponumber) = me->gl_code+5(7).
      DATA(productionorder) = NEW zcl_oee_productionorder( ).
*   Creating an instance of the object productionorder
      productionorder->set_productionorder(
              code              = ponumber
              description       = me->gl_description
              starttime         = me->gl_starttime
              priority          = me->gl_priority
              details           = me->gl_details
              location          = database_com->location_mapping( me->gl_location ) ).
*    POST Request for production order
      productionorder->productionorder_post_request( ).

*    set global variables productionorder_id and location_id so that they can be used in the post request for the position
      me->gl_po_id          = productionorder->get_productionorder_id( ).
      me->gl_location_id    = productionorder->get_location_id( ).

      DATA(product) = NEW zcl_oee_product( ).

      IF database_com->product_check( me->gl_product ) = abap_false.
          product->set_product(
                name = me->gl_product_long
                name_short = me->gl_product
                location_id = gl_location_id ).
          product->product_post_request( ).
          me->gl_product_id = product->get_product_id( ).

          database_com->insert_product(
                product_client = '202'
                product_short = me->gl_product
                product_long = me->gl_description
                product_id_oee = me->gl_product_id
           ).
      ENDIF.

*     Creating an instance of the object position
      DATA(position) = NEW zcl_oee_position( ).
      position->set_position(
        po_id               = me->gl_po_id
        location_id         = me->gl_location_id
        product             = database_com->product_mapping( me->gl_product ) "Product Mapping to get the ID that the product has in oee.ai
        step                = me->gl_step
        description         = me->gl_description
        starttime           = me->gl_starttime
        batch               = me->gl_batch
        quantity            = me->gl_quantity
        delta               = me->gl_delta
        changeoverminutes   = me->gl_changeoverminutes
        ).
*   POST Request for position
      position->position_post_request( ).
*   set global variables product_id and position id for the insert in the database table (for later updates)
      me->gl_product_id = position->get_product_id( ).
      me->gl_position_id = position->get_position_id( ).

*     INSERT created production order into database for purposes of Update and Delete
*     TIME STAMP to know when last changed
      GET TIME STAMP FIELD DATA(zv_tsl).
      DATA timestamp_string TYPE string.
      timestamp_string = CONV string( zv_tsl ).

      database_com->insert_productionorder(
        po_client = '202'
        po_code = me->gl_code
        po_description = me->gl_description
        po_starttime = me->gl_starttime
        po_priority = me->gl_priority
        po_details = me->gl_details
        po_location = me->gl_location
        po_step = me->gl_step
        po_batch = me->gl_batch
        po_quantity = me->gl_quantity
        po_delta = me->gl_delta
        po_changeoverminutes = me->gl_changeoverminutes
        po_product = me->gl_product
        po_product_id_oee = me->gl_product_id
        po_location_id_oee = me->gl_location_id
        po_po_id_oee = me->gl_po_id
        po_position_id_oee = me->gl_position_id
        po_lastchangeddate = timestamp_string
      ).
```

### update_productionorder_code

This METHOD is updating production-order and position.

```abap
DATA(database_com) = NEW zcl_oee_database_communication( ).
      DATA(productionorder) = NEW zcl_oee_productionorder( ).
*   production order gets set here
      DATA(ponumber) = me->gl_code+5(7).
      productionorder->set_productionorder(
              code              = ponumber
              description       = me->gl_description
              starttime         = me->gl_starttime
              priority          = me->gl_priority
              details           = me->gl_details
              location          = database_com->location_mapping( me->gl_location )  ).
*     PATCH Request for production order
      productionorder->set_po_po_id( database_com->po_po_id_mapping( me->gl_code ) ).
      productionorder->productionorder_patch_request( ).
      me->gl_po_id        = productionorder->get_productionorder_id( ).
      me->gl_location_id    = productionorder->get_location_id( ).

      DATA(position) = NEW zcl_oee_position( ).
*   position gets set here
      position->set_position(
        po_id               = me->gl_po_id
        location_id         = me->gl_location_id
        product             = database_com->product_mapping( me->gl_product )
        step                = me->gl_step
        description         = me->gl_description
        starttime           = me->gl_starttime
        batch               = me->gl_batch
        quantity            = me->gl_quantity
        delta               = me->gl_delta
        changeoverminutes   = me->gl_changeoverminutes
        ).
      position->set_pos_position_id( database_com->pos_position_id_mapping( me->gl_code ) ).
      position->position_patch_request( ).


      GET TIME STAMP FIELD DATA(zv_tsl).
      DATA timestamp_string TYPE string.
      timestamp_string = CONV string( zv_tsl ).
*     UPDATE created production order into database for purposes of Update and Delete
*     TIME STAMP to know when last changed
      database_com->update_productionorder(
          po_code = me->gl_code
          po_starttime = me->gl_starttime
          po_quantity = me->gl_quantity
          po_lastchangeddate = timestamp_string
      ).
```

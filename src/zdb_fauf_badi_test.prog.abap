*&---------------------------------------------------------------------*
*& Report zdb_fauf_badi_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdb_fauf_badi_test.

"DELETE FROM zcreatedpo.
"DELETE FROM zapiconnection.
DELETE FROM zproductmap where product_short = 'DXTR1000'.
"DATA: lt_productionorder2 TYPE TABLE OF zapiconnection.

"      lt_productionorder2 = VALUE #(
 "       ( client = '202' sm59_conid = 'OEE_API_POSITION' systemid = 'R36' token = 'Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr'
 "       type = 'position' url = 'https://api.preview.oee.ai/line-items/')
 "     ).

   "   INSERT zapiconnection FROM TABLE @lt_productionorder2.

      "DATA: lt_productionorder TYPE TABLE OF zapiconnection.

 "     lt_productionorder = VALUE #(
  "      ( client = '202' sm59_connectionid = 'OEE_API_POSITION' systemid = 'R36' token = 'Bearer QzSWvIbQTNPVujs8qPPnNJB3588aSaKr'
   "     type = 'position' url = 'https://api.preview.oee.ai/line-items/')
    "  ).

     " INSERT zapiconnection FROM TABLE @lt_productionorder.

" Start: Auf "Sichern"-Klick soll der Code ausgeführt werdeb

*
* let material = fauf_material
* let produktionsswerk = fauf_produktionswerk

* Mapping Material ( Material wird zur ID von oee.ai)
* Mapping Produktionswerk ( Produktionswerk wird zur ID von oee.ai)
*wenn Material oder Produktionswerk beim Mapping nicht gibt, kommt Fehlermeldung

* Daten für den FAUF
* let locations = gemappte ID von oee.ai
* let code = ID des FAUF
* let description = (not required)
* let start-time = Anfangszeit des FAUF (abändern auf Zeitformat der oee.ai) (not required)
* let priority = (not required)
* let details = json objekt (not required)
*
*Daten für die Positionen
* location id aus mapping
* produkt id aus mapping
* production order id = wird aus response gezogen
* step = 1
* description = (not required)
* quantity = Produktionsanzahl; Achtung hier muss mit Produkten gerechnet werden weil diese z.B. 200/h haben oder so
* batch = (not required)
* delta = how much over or underproduction is acceptable (0)
* changeover-minutes = planned changeover duration in minutes (0)
* details = (not required)
*
*
*FAUF per REST
*Daten für den FAUF in eine JSON
* method: POST
* headers: content-type: application/vnd.api+json; Authorization: bearer key
* body: daten_in_json
* response -> Production Order ID speichern
* response -> war positiv? dann Position
*
*Position per REST
*Daten für die Position in eine JSON
*method: POST
* headers: content-type: application/vnd.api+json; Authorization: bearer key
* body: daten_in_json
*
*
*

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in 6a6deb7ffd91a4404961510e47569e6d162488e9dfac88a86e5c12ba1b12e534#0 \
  --tx-out $(cat mathBounty.addr)+50000000 \
  --tx-out-datum-hash-file datum.json \
  --change-address $Adr07 \
  --out-file tx.unsigned

cardano-cli transaction sign  \
  --tx-body-file tx.unsigned  \
  --signing-key-file Adr07.skey  \
  $PREVIEW  \
  --out-file tx.signed

cardano-cli transaction submit  \
  $PREVIEW \
  --tx-file tx.signed
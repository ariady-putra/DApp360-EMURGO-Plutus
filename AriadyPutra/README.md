# VideoGame (as NFTs) Marketplace

It's a game-store just like Steam, but when a user purchase a game they get an NFT which acts as a game-pass. They can resell the NFTs at the secondary marketplace and the game publishers get a percentage cut from each sale.

## Document Outlines

1. [Participants](#-participants-outline)
2. [Marketplaces](#-marketplaces-outline)
3. [Actions](#-actions-outline)
4. [Scripts](#-scripts-outline)
5. [Transactions](#-transaction-outline)
6. [Logics](#-logic-outline)
7. [Potential vulnerabilities](#-potential-vulnerabilities)

## Participants Outline

1. [The platform](#-the-platform)
2. [Game devs / publishers](#-game-devs--publishers)
3. [Users (customers)](#-users-customers)

### The Platform

### Game Devs & Publishers

### Users (Customers)

[back to top](#-document-outlines)

## Marketplaces Outline

1. [Primary market](#-primary-marketplace)
2. [Secondary market](#-secondary-marketplace)

### Primary Marketplace

### Secondary Marketplace

[back to top](#-document-outlines)

## Actions Outline

1. [At the primary market](#-at-the-primary-marketplace)
  a. [Mint NFTs](#-mint-nfts-gamepass-tokens)
  b. [Update NFT prices](#-update-nft-prices)
  c. [Burn unsold NFTs](#-burn-unsold-nfts)
  d. [Request refund?](#-request-refund)
  e. [Process refund?](#-process-refund)
2. [At the secondary market](#-at-the-secondary-marketplace)
  a. [List NFT](#-list-nft)
  b. [Buy NFT](#-buy-nft)
  c. [Update NFT price](#-update-nft-price)
  d. [Cancel NFT listing](#-cancel-nft-listing)

### At the Primary Marketplace

Here, the main interactions are between game devs / publishers and the users (customers). The platform gets a percentage cut from each sale and is able to process refunds (TODO: On-chain? Off-chain? Possible?), if any.

#### Mint NFTs (GamePass Tokens)

TODO: Either the game publishers mint the NFTs first and then the users can purchase, or each user mints a token on purchase instead.

#### Update NFT Prices

Game Publisher

#### Burn Unsold NFTs

If the users mint a token on each purchase then this action does not exist, because there is no NFT stock.

#### Request refund?

User

#### Process refund?

The Platform

### At the Secondary Marketplace

Here, the interactions are between users to users. A user lists their game-pass (NFT) and then another user can purchase the token, the game publisher gets a percentage cut from each sale. The platform does not get any percentage cut from the sales at the secondary marketplace, it is created for the community.

#### List NFT

User

#### Buy NFT

User

#### Update NFT price

User

#### Cancel NFT listing

User

[back to top](#-document-outlines)

## Scripts Outline

Sell Script 

( MadeLoan Script -> Lend Script )
MadeLoan Script 

[back to top](#-document-outlines)

## Transaction Outline

### Tx 1 (List NFT)

In:
User1 (ADA + NFT)

Out:
Sell Script [ owner: Address, recipient: Address, price: Value ] (NFT)

### Tx 2 (Buy NFT [Taker]) 

In:
User2 (X ADA)
ref1 @ Sell Script [ ...recipient: Address ] (NFT)

Out:
User2 (NFT + NFT2 )
recipient [ ref1 ] (X ADA)

### Tx 3 (Lend NFT)

In:
User3 (ADA + NFT)

Out:
Lend Script [ owner: Address, borrower: Address, collateral: Value, interest: Value, duration: Interger, nft: CurrencySymbol ] (NFT)

### Tx 4 (Borrow NFT)

In:
Lend Sript [ ... ] (NFT)
User4 (X ADA)

Out:
User4 (NFT)
MadeLoan Script [ ...borrower=User4,  ]

[back to top](#-document-outlines)

## Logic Outline

### Sell Script 

Datum:
owner: Address
recipient: Address
price: Value

Redeemer:
Buy NFT

Buy NFT: 
= recipient gets price ( datum = utxoRef of self )

### Lend Script 

Datum:
owner: Address
collateral: Value
interest: Value
duration: Interger
nft: CurrencySymbol

Redeemer:
Cancel
Borrow
Update

### MadeLoan Script 

Datum:
owner: Address
borrower: Address 
interest: Value
start: Integer
duration: Interger
nft: CurrencySymbol

Redeemer:
PayBack
ClawBack

[back to top](#-document-outlines)

## Potential Vulnerabilities

Buy NFT Double Satisfaction (+)

[back to top](#-document-outlines)

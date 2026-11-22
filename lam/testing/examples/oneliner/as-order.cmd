-MShop --json --path=orders --as=Order -pe 'let Order { user, items } = json in
         f"{user}: {Shop.total items:,} yen"' examples/oneliner/shop.json

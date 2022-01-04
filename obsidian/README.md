# Program Structure and Functionality
The program has three classes: `InitFile`, `PriceList`, and `Product`:
- `InitFile` contains the `main()` method and creates an instance of `PriceList` by reading in a file that contains the prices. `main()` prompts the user for the file path and the amount on the gift card. `InitFile` also stores `filePath`, `fileName`, and any incorrectly formatted price records in `malformedInputs`.
- `Product` stores the product identifier as a `String` and its price as an `int`.
- `PriceList` contains the core functionality. It stores the list of prices in the field `ArrayList<Product> inventoryList`.  The method `getPair()` takes in the gift card amount entered by the user and systematically checks `inventoryList` for pairs of products whose price sum is closest to the gift card amount without going over. If such a pair exists, `getPair()` returns a `String` containing the two product identifiers and their prices in the format
 
    `"<ProductIdentifier1> <Price1>, <ProductIdentifier2> <Price2>"`
	
 - If no such pair exists, the method returns the string 
 
    `"Not possible"`

# Running the Program
There are two ways of running the program:
1)  Load the source into your IDE of choice and run `main()` method in the `InitFile.java` class or
2) Navigate to the project directory in the command line:

    `FindPair/target/classes` 

    and run the following command:

    `javac InitFile`.

In either case you will be presented with this prompt:

`Please enter the path to your price list file (/PATH/TO/FILE/<ListOfPrices>.txt): `

Followed by this prompt:

`Please enter the dollar value of the gift card in cents ($10.00 = 1000):`

You will then be presented with the pair of products whose price sum is closest to the gift card amount without going over (if it exists) followed by the prompt asking to try another gift card amount: 

`Try another gift card amount?`

Enter a new amount to get a new pair of products from the same price list. Enter `x` to end the program.

# Testing

I have created six test files and stored them in:

`FindPair/src/main/resources`

These include:
- `BlankFile.txt` containing no price records.
- `Malformed.txt` containing a few incorrectly formatted price records.
- `OiginalPrices.txt` containing the original price list given in the problem statement.
- `ProgressivelyMoreMinimalPrices.txt` containing tricky combinations of ever minimally gift pairs under the gift card amount of `2850`.
- `SingleItem.txt` containing only a single price record.
- `TwoItems.txt` containing only two price records.

I have written 11 tests over the files using `JUnit` with a series of `@test`s and `Assert` statements to test my code inside the test class `Test.java` found in:

`FindPair/src/test/java`

The tests can be run in your IDE of choice from inside `Test.java` or the paths to each file can be given to the `InitFile` class as above. The inputs with the expected results follow.

```
Intput: resources/OriginalPrices.txt, 2500 gift card amount
Expected output: "Candy Bar 500, Earmuffs 2000"`
```
 
```
Intput: resources/OriginalPrices.txt, 2300 gift card amount
Expected output: "Paperback Book 700, Headphones 1400"`
```

```
Intput: resources/OriginalPrices.txt, 1100 gift card amount
Expected output: "Not possible"`
```
 
```
Intput: resources/OriginalPrices.txt, 8000 gift card amount
Expected output: "Earmuffs 2000, Bluetooth Stereo 6000"`
```
 
```
Intput: resources/ProgressivelyMoreMinimalPrices.txt, 2850 gift card amount
Expected output: "ProductD 1000, ProductG 1801"`
```

```
Intput: resources/ProgressivelyMoreMinimalPrices.txt, 2900 gift card amount
Expected output: "ProductC 900, ProductH 2000"`
```

```
Intput: resources/TwoItems.txt, 210 gift card amount
Expected output: "ProduceA 10, ProduceB 200"`
```

```
Intput: resources/TwoItems.txt, 10 gift card amount
Expected output: "Not possible"`
```

``` 
Intput: resources/BlankFile.txt, 10 gift card amount
Expected output: "Not possible"`
```

``` 
Intput: resources/Malformed.txt, 6500 gift card amount
Expected output: "Candy Bar 500, Bluetooth Stereo 6000"``
``` 
 
``` 
Intput: resources/Malformed.txt, 2400 gift card amount
Expected output: "Not possible"`
``` 

# Representation

# icTranslation (javaToProlog)
- `ProgramModel.java` class in `ai.phasechange.javatoprolog.application.model`
	- Has a DOB field that is initialized when an instance of `ProgramModel` gets built in `ProgramBuilder.java`. 
- `ProgramBuilder.java` class in `ai.phasechange.javatoprolog.application.model.builder` 
	- The `build()` method gets the DOB from the `RepresentationController` instance and sets the DOB field in the `ProgramModel` instance the builder is building.
- `ProgramTranslator.java` class in `ai.phasechange.javatoprolog.application.translator`. 
	- The `translateProgram()` method calls the `getDob()` method on the `ProgramModel` instance `programModel`.
- `DobSourceCodeBuilder.java` class in `ai.phasechange.javatoprolog.application.model.builder`
	- Has a field that is a `RepresentationController` instance called `contoller`. This gets set in the class constructor.
	- The `build()` method gets passed a list of DOBs.
- `RepresentationControllerTest.java` class in `ai.phasechange.javatoprolog.application.controller`
	- Uses different aspects of the DOB in many tests.
	
# icGeneration
- `InitHandler.java` class in `ai.phasechange.icgenrator.requesthandler.init`.
	- Instantiates an instance of `RepresentationController` as a class field.
	- The `createArtifacts()` method creates a dob from Json using an instance of `ReasoningSerializer` called `serilizer`. The same method also constructs an `EnhancedDob` instance by passing it in the dob constructed from the Json.
	- The `pullDob()` method uses the `getDob()` method of `representationController` class field to instantiate a DOB instance.
- `EnhancedDobBuilder.java` class in `ai.phasechange.icgenrator.requesthandler.init.builder`
	- Has a `ReasoningCommons` DOB class field that gets set in the class's constructor and used throughout the class.
	- Anything that creates an instance of `EnhancedDobBuilder.java` is touching the DOB at some point.
- `ApplicationsUtil.java` class in `ai.phasechange.icgenrator.requesthandler.init`
	- Instantiates an instance of `RepresentationController` as a class field.
	- `pullDob()` method instantiates a `ReasoningCommons` DOB using the `getDob()` method of the `representationController` class field.
	- The `createArtifacts()` method creates a dob from Json using an instance of `ReasoningSerializer` called `serilizer`. The same method also constructs an `EnhancedDob` instance by passing it in the dob constructed from the Json.
- `EnhancedDobVizTests.java` class in `ai.phasechange.icgenerator.requesthandler.util`
	- `init()` constructs a DOB from Json.
- All classes that reference `EnhancedDob`:
	- `ApplicationsUtil.java`
	- `InitHandler.java`
	- `CausalCategorizer.java`
	- `EnhancedDobBuilder.java`
	- `FaucSignatureBuilder.java`
	- `FaucsBuilder.java`
	- `FieldToStateFunctionBuilder.java`
	- `InputFieldEffectsBuilder.java`
	- `StateFunctionSignatureBuilder.java`
	- `DobSlicer.java`
	- `EnhancedDob.java`
	- `EnhancedDobViz.java`
	- `CausalCategorizerTests.java`
	- `EnhancedDobTests.java`
	- `FaucBuilderTest.java`
	- `FaucSignatureBuilderTests.java`
	- `FaucStateFunctionTests.java`
	- `DOBSlicerTests.java`
	- `EnhancedDobVizTests.java`
	
# LeastModel
- ???: I think this only references state function information.

# Query
- `RepresentationClient.java` class in `ai.phasechange.query.client`
	- Instantiates `RepresentationController` from `ReasoningCommons` but nowhere is `getDob()` called.
- `RepresentationProvider.java` class in `ai.phasechange.query.client`
	- Instantiates `RepresentationController` from `ReasoningCommons` but nowhere is `getDob()` called.
- `DependencyGraphBuilder.java` class in `ai.phasechange.query.groundprojection`
	- Has several data structures and methods that use DOB nodes. For example the `buildDependencyGraph()` method uses the DOB nodes from the `mergedExecutionPath` data structure.
- `TestHelper.java` in `ai.phasechange.query`
	- Makes several references to PCIC's DOBs.
	
# SymbolicExecutor
- Basically everything about this will have to change. Designed to execute over DOB graphs. New SE will execute over FOM programs.

# UI-Adapter
???
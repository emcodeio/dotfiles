# Representation
- Everything after SSA-CFG construction.
- JCL-DOB?
- `PipelineArtifactPopulator.java` class in `ai.phasechange.server`
	- Serializes the artifacts created by Representation to make them available to other services. This will break if a DOB is not produced.
- Meeting notes
	- What about JCL-DOB?
	- Serialization, can turn it off? Refactor to not require all objects be constructed.
	
# icGeneration
- Builds OPUCs, FAUCs, and State Functions from DOBs
- Code Associated with this functionality.
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
	
# icTranslation (javaToProlog)
- Builds a model of the program from the DOB and translates it to Prolog.
- Code associated with this functionality.
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
- Meeting notes
	- FOM provides function information that the DOB captures
	- Firm contract coming out of icGen. Can FOM provide everything in that contract?
	- Source code builder. How to map back to source?
	- JCL-DOB
# Query
- Does not use DOB directly. Uses DOB nodes to store executed nodes received from SE.
- Code that instantiates `RepresentationController` or references DOB nodes.
	- `RepresentationClient.java` class in `ai.phasechange.query.client`
		- Instantiates `RepresentationController` from `ReasoningCommons` but nowhere is `getDob()` called.
	- `RepresentationProvider.java` class in `ai.phasechange.query.client`
		- Instantiates `RepresentationController` from `ReasoningCommons` but nowhere is `getDob()` called.
	- `DependencyGraphBuilder.java` class in `ai.phasechange.query.groundprojection`
		- THIS IS BIGGER (DRAGON): Has several data structures and methods that use DOB nodes. For example the `buildDependencyGraph()` method uses the DOB nodes from the `mergedExecutionPath` data structure.
	- `TestHelper.java` in `ai.phasechange.query`
		- Makes several references to PCIC's DOBs.
- Meeting notes
	- Is DependencyGraphBuilder using any dependencies *between* DOB nodes.
# LeastModel
- ???: I think this only references state function information.

# SymbolicExecutor
- Basically everything about this will have to change. Designed to execute over DOB graphs. New SE will execute over FOM programs.

# UI-Adapter
- Uses the DOB directly to build source annotations.
- Code associated with that functionality.
	- `UiaInitCallBacks.java` class in `ai.phasechange.uiadapter.server`
		- Instantiates `RepresentationControllerd` from `ReasoningCommons`. The `initializeApplication()` method to call the `getDobLookup()` method on this instance to pull in the DOB. Passes it to `SourceHandler` instance.
	- `SourceHandler.java` class in `ai.phasechange.uiadapter.requesthandler.sources`
		- Has a `DobLookup` class field that gets initialized in the class constructor and used in the `handleRequest()` method.
	- `SourceBuilder.java` class in `ai.phasechange.uiadapter.requesthandler.sources.builder`
		- Has a `DobLookup` class field that gets initialized in the class constructor. The `constructSourceLines()` method uses this field when called.
	- `SourceAnnotationAdapter.java` class in `ai.phasechange.uiadapter.graphql.adapter.applications`
		- Has a `List<Dob>` class field that gets initialized in the class constructor. This field is used throughout the methods of this class.
- Meeting notes
	- Lots of dragons here.
- General Notes
	- List of requirements of FOM and icGen expanded concepts.

# Meeting Videos
[2022.02.16: FOM 4+ Discussion](https://iosemantics-my.sharepoint.com/:v:/g/personal/sbucuvalas_phasechange_ai/ER0pb69YpiFPvy516k1jD6IBapvtP_2bhiGKQ4E_qo3K8w)
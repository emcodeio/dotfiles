# Tell Me About Yourself 
## Who I am

My name is Evan and I have worked in program analysis as a software engineer and researcher for about 6 years.

## Highlight reel

I've developed many representations and reasoning techniques in AI-assisted software development, I've worked closely with large financial companies to help them better understand the functionality of their legacy systems, and I love working with and learning from really smart, curious, and compassionate mentors and colleagues.

## Why I'm here.

I'm interested in Amazon because the career growth opportunities you offer and I want to get more experience working at very large scales. And it does get much larger scale than Amazon.

# Solved a big problem
## Defined FOM

### S.ituation:
1.  **Response Time too slow.**

    Clients were requesting faster response times from our system. The system allowed a user to query information about path conditions through a program and the system would return feasablility data.

### T.ask:
1.  **Figure out a way to speed up analysis.**

    Response times were a function of the analysis being run on the backend of the system. My job was to figure out a way to speed up this analysis.

### A.ction:
1.  **Transpiled COBOL to a Functional Language.**

    Inspired by work on functional programming languages, I worked with a team of four to develop a transpiler that would translate COBOL source code into a functional langauge called FOM, or First Order Model. The ease of analysis of functional langauges was a primary design decision.
	
### R.esult:
1.  **80% increase in analysis. Positive client feedback.**

    The use of FOM during back-end analysis increased analysis by 80% and we recieved overwhelmingly positive feedback from out clients.

## Implamented the linearizer
### S.ituation:
1.  **Needed to speed up a criticacl functionality in system (Reduce SE conditionals)**

    My company was developing a system whose most expensive process was symbolic execution. Symbolic execution was critical for the type of functionality our customers where expecting, but the only way to really speed up symbolic execution is to reduce the number of conditions that are executed.

### T.ask:
1.  **Design and implement linearizer**

    My task was to design and implement a linearizer that executed a slice of a program in the original order of the source.

### A.ction:
1.  **Designed a graph stitching algorithm from orignal graph**

    The slices were represented as graphs so I developed an algorithm that would walk the graph representing the entire program in order to stitch together the slice's nodes to produce a correct execution order.

### R.esult:
1.  **Slicer and linearizer reduced conditionals by 75%. 50% increase in SE**

    The slicer along with the linearizer reduced the number of conditionals that needed to be executed by 75%, increasing the speed of symbolic execution by 50% in most cases.

## Defined the slicers
### S.ituation:
1.  **Needed to speed up a criticacl functionality in system (Reduce SE conditionals)**

    My company was developing a system whose most expensive process was symbolic execution. Symbolic execution was critical for the type of functionality our customers where expecting, but the only way to really speed up symbolic execution is to reduce the number of conditions that are executed.

### T.ask:
1.  **Figure out way to reduce number of statements**

    My task was to develop methods for statically reducing the number of statements in a program slice to a minimal set so the minimum amount of information from the original program needed to be symbolicly executed.

### A.ction:
1.  **Designed, prototyped, and implemented slicing techneques**

    I worked closely in a team of three to design, prototype, and implement various representations, analyses, and slicing techniques based on input/output behavior of the program. The thinking was that we could reduce the amount of information in the slice by focusing in on how the program transformed the data, particularly data coming from outside the program.

### R.esult:
1.  **Slicer and linearizer reduced conditionsl by 75%. 50% increase in SE**

    The slicer along with the linearizer reduced the number of conditionals that needed to be executed by 75%, increasing the speed of symbolic execution by 50% in most cases.

# Difficult Person & How I handled it
## Worked with a colleague who had a tendency to consider clarifying questions as challenges.
### S.ituation:
1.  **Formalizing a graph rep, colleague and I disagreed about edges**

### T.ask:
1.  **Figure out best way to come to agreement**

### A.ction:
1.  **Anchored at problem, small examples, open and honest listening**

### R.esult:
1.  **75% reduction in edges, patent, fruitful working relationship.**

# Messed up or failed
## Backward Goto elimination
### S.ituation:
1.  **Team of which I was a lead drifted from design. Signed off on drift not knowing.**

    While implementing a method to transform backward gotos into explicit loop structures, the team I was leading drifted from the original design. This drift was seen as significantly jeapordizing the success of the implementation due to the difficulty of such transformations. I signed off on those drifts, not realizing what was happening and it was now too late to undo our work.

### T.ask:
1.  **Own mistake, figure out scope of drift, risks.**

    My job was to own the mistake and to work closely with my team to understand the nature of the drift and assess the potential risks.

### A.ction:
1.  **Back to original designs, precise list of differences, discoved low risk.**

    My team and I went back to the original designs, figured out exactly where the differences were between it and our implementation and showed that while some of the details were different, the implementation was still consistent with the original intent.

### R.esult:
1.  **Finished impl, tested correclty, client thrilled with performance.**

    After showing that we were still consistent with the goals of the design, the implementation was finished and testing showed it produced correct results. The clients heavily using that part of the system were thrilled with it's performance.

## Adjusted onboarding strategies
### S.ituation:
1.  **Back numbers and negative feedback from onboarding process.**

    I often served as the first point of contact for new hires during their training process. The idea was to give them a high-level overview of the pipeline and the intuitions about the functioning of each piece before diving into the details of the system. The results and feedback from the first few people I engaged with were solidly negative.

### T.ask:
1.  **Adjust and iterate.**

    My taks was to adjust and interate on my approach to decrease new hire confusion and shorten their time to productivity.

### A.ction:
1.  **Building bridge from current understanding. Convo instead of info dump.**

    I started out by considering situations in which I learned best and realized it was when I could easily build a bridge between my current level of understanding to the higher level. I decided to stop dumping too much information on the new hire and opted instead to try to understand their current level of knowledge first. Then I tried to help them build that bridge.

### R.esult:
1.  **50% decrease in ramp-up time. Positive feedback.**

    Contributed to a 50% decrease in rampup time to productivity for new hires. Also recieved overwhelmingly positive feedback from new hires after adjusting to new process.

# A Time I'm Proud Of
## Defined the Dob
### S.ituation:
1.  **Struggled to find right DOB formalism.**

    The company was struggling to formalize a mission critial representation. We understood what we wanted from the representaion in many specific example, but lacked a unifying principle that generalized.

### T.ask:
1.  **Come up with general framework.**

    My task was to come up with that generalized framework that would formalism the representation.

### A.ction:
1.  **Month with team. Pulled from many places. Theory of Causal Models.**

    Spent a month with a team of three others pulling from many different formal theories from inside and outside program analysis. We settled on Judea Pearl's theory of causal models and applied it to how programs transform information.

### R.esult:
1.  **Reduced 75% of code, covered all cases, got a patent.**

    What came out of that work was a patented graph representation that allowed us to reduce the number of connections in a program by up to 75% while maintaining equivalence to how the original program transformed information.  patent for the work on which my name is included as a contributor. My name is included on the patent.

## Proved myself during "test" period
## S.ituation:
1.  **Little formal programming experience, company gave me a chance.**

    I was right out of college with my degree in mathematics and I was hired at Phase
    Change Software, my current company. I had focused on the mathematics of computer science, but had little formal expereince to prove my cs and programming ability in the workplace. Despite this, the people at the company saw something in me and hired me for a 6 month &ldquo;test&rdquo; period to see if I could do the work.

### T.ask:
1.  **Rise to the occasion and prove I could do the work**

    My task was to rise to the occasion and prove that I could meaningfully contribute to achieving the companies goals. I had to learn the funamentals of the programming analysis and compiler techniques used in the companies IP to the extend that I could solve meaningful problems

### A.ction:
1.  **Personal time spent in texbooks and online material. Worked with smart people.**

    I dove into many textbooks and online courses on my own time while working closely with the most knowledgeable people at the company to get up to speed quickly.

### R.esult:
1.  **Beyond expectations, 20% bump in pay, promoted to Research Engineer**

    At the end of the 6 months, the company valued my work so much that they gave me a 20% bump in pay and promoted me to Research Engineer. They said that I had gone above and beyond what they had expected.

## Eliminated Gotos
### S.ituation:
### T.ask:
### A.ction:
### R.esult:


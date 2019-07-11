# Before You Begin


<h3>Overview</h3>

 <p>    Your last assignment for the semester will be a pure functional programming language
based on Haskell. You will create several interacting pieces of code which will, at minimum, 
parse and interpret expressions in the language, providing dynamic type checking and a number
of other features (e.g., data structures) which will make the language a minimal, but realistic
language in which you could write useful programs. Additional features will provide extended
	 functionality to make your language more flexible, useful, and productive for realistic
	 programming in the pure functional style. 
	
</p>
<p>
In future semesters, CS 320 will receive Hub units for teamwork and for creativity, and I intend
	to make this project a centerpiece of those activities. Hence, there will be an emphasis on
	team development of the project (though for this semester you may work alone) and a 
	context for integrating all that you have done this
	semester into a creative experience which hopefully will give you a taste of what real language
	designers do. 
</p>
	
<p> <b>This document will be updated as we work out some of the details; for now we are providing
	a description of the basic features required by all submissions (the "vanilla" version) so that you can get
	started if you wish; the other details will be worked out by the end of this week, so that you can
	start in earnest after the second midterm. Places where further information will be provided are marked by the
	abbreviation TBA (= "To Be Announced"). 
	</b> </p>
  
  

<dt><b> Team Organization </b></dt>
<p>
<dd><ul style="list-style-type:circle;">
<li> You are strongly encouraged to do this in a team, but may do it by yourself if you prefer. No teams of more than
three will be allowed.  
</li> 
<li>In the first milestone (see below) you must answer some questions about your team and the plan for completing the project by the deadline. 
</li>   
<li> Part of the final submission will be a "360 review" (such as performed in CS 411) in which each member will submit
an <i> anonymous </i> report/reflection on 
how the team performed and any issues that we need to know about when evaluating the final project. 
</li>
<li>  In the usual case, all team members will receive the same score out of 100 points, which will then be scaled to 5% of the final grade; details about the grading TBA, but it will involve a combination of a short written summary report, various deliverables, and
your  tests on your code. We will not be providing tests, as we don't know what your syntax is, but we will give you guidance about how to test it using hunit tests (used for the automated testing this term). 
</li>
<li> In the pathological case that there are problems with very unequal contribution to the final product, we reserve the
right to give reduced or no points to members who did not fulfill their obligation to the project; in such a case every
attempt will be made to make sure that the other members do not suffer a grading penalty because of the irresponsibility
of another team  member. 
</li>
</ul>
</dd></p>
</dl>	
	
<h3>How to Start </h3>

Wow, this is overwhelming! Where to start? <b> Develop a plan and a schedule.</b>  Find a time ASAP when you and your team
can meet with no distractions for at least an hour.  Look over the whole assignment together and think about what
the vanilla project looks like, and on whose version of the last homework you will base it. Then make a rough
decision on what mix-in features are interesting and possible for you and your team to do.  Don't worry if you
don't understand all the issues; we'll help you with that.  But you do what to choose features that at least
seem interesting. Finally, decide who will do what, and the time frame for getting it all done. Yes, you may
be unrealistic in some of the deadlines, but you <b>need a plan and a schedule. The outcome of this first meeting
should be the completion of the First Milestone. </b>  

Please pay careful attention to this piece of advice: <b>Do NOT think you can pull this all together
a couple of days before the final deadline. Trust us, you will fail, and we'll be busy helping
students who took this advice seriously to finish up their projects, and will not be able to get our help with things
you should have done the previous week. To be brutally honest: none of us will have any interest close to the deadline
	in helping teams that didn't take this seriously and are going to fail the project anyway. </b>


<p> How to assign tasks to people?  Roughly, the point values of the various features correspond to how much
	work is involved, but what about the basic, vanilla project? What about testing? The major components can be see by looking at the basic project/src repo. You should organize your plan around completing these
files first, starting with the AST, and then dividing up the Parser/Helpshow and the files related to
	evaluation. The AST ties together the parsing phase and the evaluator (with a bit of static checking in between).
	These can be done in parallel and put together later. The parser is not that much work (unless you choose to do
	parser error reporting as an additional feature) so it would not be fair for one person to do the parser
	and the others to do the evaluator. Otherwise a good model is that (i) someone does the parser and Helpshow,
	(ii) everyone needs to know about
	and be involving in thinking about all the vanilla features, even if specific people are in charge of
	different phases of the coding; the person doing the parser can be responsible for less of this.   
	When you discuss the additional features, then you can break this
	up into different simultaneous activities better. 
	</p>
	


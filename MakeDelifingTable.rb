#infiled:
#ped: off sire dam cohort
#surv: BirdID lastSeenYear lastSeenMonth

#outfiles:
#	gdelifingCohort.txt
#outfile structure: cohort	Nt	Nt1 avSurv(par)	av#recrPerP	avFit	BirdID	sex	age recruits	surv		Eti	sti	fti	pti	

require 'set'

# class for all individuals (i.e. everything with an id)
class Individual
	attr_accessor :iid,		# id (has to be iid, id exists) 
		:sire, :dam, 		# parents
		:cohort, 			# year of birth
		:lsYear, :lsMonth, 	# last seen
		:chicks, 			# list of offspring (ids)
		:firstBreed, 		# year of first reproduction
		:sex
	
	def initialize(iid)
		@iid = iid
		@chicks = []
		end

	def addOffspring(i)
		##Julia: i ist hier das offspring, und i.cohort offsprings cohort, richtig?
		@chicks << i		# append to list

		@firstBreed ||= i.cohort

		if i.cohort < @firstBreed
			@firstBreed = i.cohort
			end
		end

	# whether the individual survived the first season
	def recruit
		##Julia: hier eventl. noch eine if schleife rein?
			##Julia if @lsYear ! =nil
			##Julia end
		@lsYear > @cohort
		end

	def age(year)
		@cohort ? year - @cohort : "NA"
		end
		##Julia irgendwas mit age ist auch komisch, - die sind alle negative Jahre alt...
		
	# whether alife in year
	def alife?(year)
		if @lsYear == nil
			$stderr.puts @iid.to_s + " has no last seen date"
			return nil
			end

		@lsYear > year || (@lsYear == year && @lsMonth > 3)
		end

	# whether the individual reproduced this year
	def present?(year)
		@chicks.select { |c| c.cohort == year }.length > 0
		end
				
		
	# number of surviving offspring in year
	def recruits(year)
		@chicks.select { |c| c.cohort == year && c.alife?(year + 1) }.length
		end
	end
	

# get filenames from command line
pedfName = ARGV[0]
survfName = ARGV[1]

# open both files and read the content into arrays (1 elem per line)
ped = File.open(pedfName, "r").readlines
surv = File.open(survfName, "r").readlines

# skip first line (header)
ped.shift
surv.shift

# list of individuals by id
pop = Hash.new
# list of cohorts (array of integer)
cohorts = []

# add all offspring and parents to list
ped.each do |line|
	# chop line into fields
	# l is an array of fields (as integer values)
	l = line.chomp.split.collect{ |e| e == "NA" ? nil : e.to_i}
	
	# assign all fields to values
	off, sire, dam, cohort = *l
	
	# create individual object, set properties
	pop[off] ||= Individual.new(off)
	pop[off].sire = sire
	pop[off].dam = dam
	pop[off].cohort = cohort

	# we have to keep a list of all cohorts
	cohorts << cohort

	# add the parents (if they are known) to the list
	#
	if sire != nil
		pop[sire] ||= Individual.new(sire)
		pop[sire].addOffspring(pop[off])
		pop[sire].sex = 1
		end

	if dam != nil
		pop[dam] ||= Individual.new(dam)
		pop[dam].addOffspring(pop[off])
		pop[dam].sex = 0
		end
	end

# print number of known ids
puts pop.length
puts "cohort	Nt	Nt1	avAnnSurv	avAnnRec	avW	BirdID	sex	age	surv	recruits	Eti	sti	fti	pti"	

# record survival (if known)
surv.each do |line|
	# as before, get fields from line, assign to single variables
	l = line.chomp.split.collect{ |e| e == "NA" ? nil : e.to_i}
	# assign all fields to values
	id, lsy, lsm = *l

	# this means the id didn't occur in ped
	if pop[id] == nil
#		$stderr.puts "id " + id.to_s + " missing"
		next
		end

	pop[id].lsYear = lsy
	pop[id].lsMonth = lsm
	end

# sort and get rid of duplicates
cohorts.sort!
cohorts.uniq!


# since we are going to need n_(t+1) we have to calculate this before doing
# the rest
allnt = Hash.new
cohorts.each do |coh|
	allnt[coh] = pop.select{|id, i| 
				i.present?(coh) && i.lsYear != nil}.length
	end


	
cohorts.each do |coh|
	# we know these already
	nt = allnt[coh]
	nt_next = allnt[coh+1] || "NA"
	
	# collect all individuals that reproduced successfully this year
	parents = []
	pop.each do |id, i| 
		# check whether i produced a recruit
		if i.present?(coh) 
			# if there's no last seen date that's an error
			if i.lsYear == nil
				$stderr.puts id.to_s + " parent has no last seen date"
			else
				# add to list
				parents << i 
				end
			end
		end

	# calculate cohort stuff

	avSurv = parents.inject(0){|sum, p| 
		sum + (p.alife?(coh+1) ? 1 : 0)}.to_f / parents.length.to_f

#	equivalent to:
#	sum = 0
#	parents.each{|p| 
#		sum += (p.alife?(coh+1) ? 1 : 0)}
#	avSurv = sum.to_f / parents.length.to_f

	avRecr = parents.inject(0){|sum, p|
		sum + p.recruits(coh)}.to_f / parents.length.to_f
	
	avFit = avSurv + avRecr


	# calculate individual stuff

	asti = 5.0
	afti = 5.0
	apti = 5.0
	
	
	parents.each do |parent|
		print coh, "\t", nt, "\t", nt_next, "\t", avSurv, "\t",
			avRecr, "\t", avFit, "\t"

		surv = parent.alife?(coh+1) ? 1 : 0

		print parent.iid, "\t", parent.sex,"\t", parent.age(coh), "\t",
			surv, "\t", parent.recruits(coh), "\t"
		
		print (surv + parent.recruits(coh).to_f), "\t"
			
		print (surv.to_f - avSurv)/(nt - 1), "\t"
		asti +=(surv.to_f - avSurv)/(nt - 1)
		
		print (parent.recruits(coh).to_f - avRecr)/(nt - 1), "\t"
		afti +=	(parent.recruits(coh).to_f - avRecr)/(nt - 1)
		
		if nt_next != "NA"
			print	((surv + parent.recruits(coh).to_f)- avFit)/(nt - 1)
			apti += ((surv + parent.recruits(coh).to_f)- avFit)/(nt - 1)
		else
			print "NA", "\t"
			end
		puts
		end
		
	$stderr.print coh, "\t", asti/parents.length, "\t",afti/parents.length, "\t", apti/parents.length
	$stderr.puts
	
	end

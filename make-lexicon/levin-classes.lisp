
;;; Copyright (C) 2021-present  Sony Computer Science Laboratories Paris
;;;                             Remi van Trijp (www.remivantrijp.eu)
;;; 
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation version 3 of the License.
;;; 
;;;     This program is distributed in the hope that it will be useful
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;; 
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not see <https://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------------

(ql:quickload :beng)

;;; ============================================================================
;;; VERB CLASSES
;;; ============================================================================

(defparameter *levin-verb-classes* (make-hash-table) "Verb classes defined by Beth Levin's book.")
(defstruct verb-class name type members examples)

(defmacro define-verb-class (name &key type members examples (container '*levin-verb-classes*))
  `(let ((verb-class (make-verb-class :name ',name
                                      :type ',type
                                      :members ',members
                                      :examples ',examples)))
     ;; We will also store an entry for the verb in our container with information about
     ;; its class membership.
     (dolist (member ',members)
       (if (listp member)
         (dolist (sub-member (rest member))
           (pushnew ',name (gethash sub-member ,container)))
         (pushnew ',name (gethash member ,container))))
     ;; Then store the verb class itself.
     (setf (gethash ',name ,container) verb-class)))
  
(define-verb-class putting-verbs
                   :type putting
                   :members (arrange immerse install lodge mount place position put set
                                     situate sling stash stow)
                   :examples ("I put the book on the table."
                              "I immersed the paper in water."
                              "I installed the software on my computer."
                              "I mounted a campaign in Europe."
                              "I placed the book on the table."
                              "I positioned the book on the table."
                              "I set the book back on the table."
                              "I situated the book in the library."
                              "I slinged the garbage into the trash bin."
                              "I stowed my luggage into the overhead compartment."))

(define-verb-class putting-spatial-verbs
                   :type putting
                   :members (dangle hang lay lean perch rest sit stand suspend)
                   :examples ("She stood the books on the shelf."))

(define-verb-class funnel-verbs
                   :type putting
                   :members (bang channel dip dump funnel hammer ladle pound push rake ram scoop
                                  scrape shake shovel siphon spoon squeeze squish squash sweep
                                  tuck wad wedge wipe wring)
                   :examples ("I funneled the mixture into the bottle."))

(define-verb-class putting-directional-verbs
                   :type putting
                   :members (drop hoist lift lower raise)
                   :examples ("I lifted the books."
                              "I lifted the books from the floor to the table."))

(define-verb-class pour-verbs
                   :type putting
                   :members (dribble drip pour slop slosh spew spill spurt)
                   :examples ("She poured water over the flowers."
                              "She poured water into the bowl."
                              "She poured water from the pitcher."
                              "She poured water out of the pitcher."))

(define-verb-class coil-verbs
                   :type putting
                   :members (coil curl loop roll spin twirl twist whirl wind)
                   :examples ("She coiled the rope around the post."
                              "The rope coiled around the post."
                              "That kind of rope coils easily around the post."
                              "Water poured onto the plants."))

(define-verb-class spray-load-verbs
                   :type putting
                   :members (brush cram crowd cultivate dab daub drape drizzle dust hang heap
                                  inject jam load mound pack pile plant plaster pump rub scatter
                                  seed settle sew shower slather smear smudge sow spatter splash
                                  splatter spray spread sprinkle spritz squirt stack stick stock
                                  strew string stuff swab)
                   :examples ("She loaded boxes onto the wagon."
                              "She loaded boxes into the wagon."
                              "She loaded boxes under the wagon."
                              "She sprayed paint over the table."
                              "She sprayed paint on the wall."
                              "She sprayed the wall with paint."
                              "Paint sprayed on the wall."
                              "She splashed water at me."
                              "She rubbed the lotion on herself."))

(define-verb-class fill-verbs
                   :type putting
                   :members (adorn anoint bandage bathe bestrew bind blanket block blot bombard
                                  carpet choke cloak clog clutter coat contaminate cover dam dapple
                                  deck decorate deluge dirty douse dot drench edge embellish
                                  emblazon encircle encrust endow enrich entangle face festoon fill fleck
                                  flood frame garland garnish imbue impregnate infect inlay interlace
                                  interlard interleave intersperse interweave inundate lard lash line litter
                                  mask mottle ornament pad pave plate plug pollute replenish repopulate
                                  riddle ring ripple robe saturate season shroud smother soak soil speckle
                                  splotch spot staff stain stipple stop up stud suffuse surround swaddle
                                  swathe taint tile trim veil vein wreathe)
                   :examples ("She staffed the store with employees."
                              "The employees staffed the store."
                              "She swaddled the baby with blankets."
                              "She swaddled the baby in blankets."))

(define-verb-class butter-verbs
                   :type putting
                   :members (asphalt bait blanket blindfold board bread brick bridle bronze
                                    butter buttonhole cap carpet caulk chrome cloak cork crown
                                    diaper drug feather fence flour forest frame fuel gag garland
                                    glove graffiti gravel grease groove halter harness heel ink label
                                    leash leaven lipstick mantle mulch muzzle nickel oil ornament
                                    panel paper parquet patch pepper perfume pitch plank plaster poison
                                    polish pomade poster postmark powder putty robe roof rosin rouge rut
                                    saddle salt salve sand seed sequin shawl shingle shoe shutter silver
                                    slate slipcover sod sole spice stain starch stopper stress string stucco
                                    sugar sulphur tag tar tarmac tassel thatch ticket tile turf veil veneer
                                    wallpaper water wax whitewash wreathe yoke zipcode)
                   :examples ("She buttered the toast."
                              "She buttered her toast with unsalted butter."))

(define-verb-class pocket-verbs
                   :type putting
                   :members (archive bag bank beach bed bench berth billet bin bottle box cage can
                                     case cellar cloister coop corral crate dock drydock file fork garage
                                     ground hangar house jail jar jug kennel land lodge pasture pen pillory
                                     pocket pot sheathe shelter shelve shoulder skewer snare spindle spit spool
                                     stable string tin trap tree warehouse)
                   :examples ("She pocketed the change."))

(define-verb-class remove-verbs
                   :type removing
                   :members (abstract cull delete discharge disgorge dislodge dismiss disengage draw eject
                                     eliminate eradicate evict excise excommunicate expel extirpate extract
                                     extrude lop omit ostracize oust partition pry reap remove separate
                                     sever shoo subtract uproot winkle withdraw wrench)
                   :examples ("He removed the smudges from the tabletop."))

(define-verb-class banish-verbs
                   :type removing
                   :members (banish deport evacuate expel extradite recall remove)
                   :examples ("He banished the general from the army."))

(define-verb-class clear-verbs
                   :type removing
                   :members (clear clean drain empty)
                   :examples ("He cleared the dishes from the table."
                              "He cleared the dishes from under the sink."
                              "He cleared the table."))

(define-verb-class wipe-verbs
                   :type removing
                   :members ((:manner bail buff dab distill dust erase expunge flush leach
                              lick pluck polish prune purge rinse rub scour scrape
                              scratch scrub shave skim smooth soak squeeze strain strip
                              suck suction swab sweep trim wash wear weed whisk winnow
                              wipe wring)
                             (:instrument brush comb file filter hoover hose iron mop
                              plow rake sandpaper shear shovel siphon sponge towel vacuum))
                   :examples ("He wiped his fingerprints from the counter."
                              "He wiped his finger prints from under the cupboard."
                              "He wiped the counter."))

(define-verb-class steal-verbs ;; Remove possession
                   :type removing
                   :members (abduct cadge capture confiscate cop emancipate embezzle exorcise
                                   extort extract filch flog grab impound kidnap liberate lift
                                   nab pilfer pinch pirate plagiarize purloin recover redeem reclaim
                                   regain possess rescue retrieve rustle seize smuggle snatch sneak
                                   sponge steal swipe take thieve wangle weasel winkle withdraw wrest)
                   :examples ("The thief stole the painting from the museum."
                              "The thief stole the painting."
                              "The thief stole the painting for his friend."))

(define-verb-class cheat-verbs ;; Possessional deprivation
                   :type removing
                   :members (absolve acquit balk bereave bilk bleed burgle cheat cleanse
                                    con cull cure defraud denude deplete depopulate deprive
                                    despoil disabuse disarm disencumber dispossess divest drain
                                    ease exonerate fleece free gull milk mulct pardon plunder
                                    purge purify ransack relieve render rid rifle rob sap
                                    strip swindle unburden void wean)
                   :examples ("The doctor cured him of pneumonia."
                              "The doctore cured him."
                              "The swindler cheated him out of his fortune."))

(define-verb-class pit-verbs
                   :type removing
                   :members (bark beard bone burl core gill gut head hull husk lint louse milk peel
                                  pinion pip pit pith pod poll pulp rind scale scalp seed shell shuck
                                  skin snail stalk stem stone string tail tassel top vein weed wind worm zest)
                   :examples ("The cook boned the fish."))

(define-verb-class debone-verbs
                   :type removing
                   :members (deaccent debark debone debowel debug debur declaw defang defat defeather deflea
                                      deflesh defoam defog deforest defrost defuzz degas degerm deglaze degrease
                                      degrit degum degut dehair dehead dehorn dehull dehusk deice deink delint
                                      delouse deluster demast derat derib derind desalt descale desex desprout
                                      destarch des tress detassel detusk devein dewater dewax deworm)
                   :examples ("The cook deboned the fish."))

(define-verb-class mine-verbs
                   :type removing
                   :members (mine quarry)
                   :examples ("They mined gold from the mountain."
                              "They mined the gold."))

(define-verb-class send-verbs
                   :type send-carry
                   :members (airmail convey deliver dispatch express forward hand mail pass port post
                                     return send shift ship shunt slip smuggle sneak transfer transport)
                   :examples ("She sent the book from Paris to London."
                              "She sent him the book."
                              "She sent the book."
                              "She sent the book to him."))

(define-verb-class slide-verbs
                   :type send-carry
                   :members (bounce float move roll slide)
                   :examples ("She slid the books across the table."
                              "She rolled the ball from one end of the table to the other."
                              "She slid the book to him."
                              "She rolled the ball."
                              "The ball rolled across the table."
                              "Those books slide across the table easily."))

(define-verb-class bring-take-verbs
                   :type send-carry
                   :members (bring take)
                   :examples ("She brought a book to the meeting."
                               "She brought a book."
                               "She brought him a book."))
                              
                              
                              
                   


                         
                              
                              


                              



;; (verbs-of-putting)

;; "I put the book on/under/near the table."
;; *I put the book to Sally.
;; 


;;; ============================================================================
;;; 1. Transitivity Alternations
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; 1.1 SP-alignment
;;; ----------------------------------------------------------------------------

;;; 1.1.1 Middle Alternation
;;; "The butcher cuts the meat."
;;; "The meat cuts easily."
;;;
;;; 1.1.2 Causative Alternation
;;;

(defun roll-verbs ()
  '(bounce drift drop float glide move roll slide swing
           ;; Also around an axis:
           coil revolve rotate spin tum twirl twist whirl wind))
;; (roll-verbs)

(defun break-verbs ()
  '(break chip crack crash crush fracture rip shatter smash snap splinter split tear))
;; (break-verbs)

(defun bend-verbs ()
  '(bend crease crinkle crumple fold rumple wrinkle))
;; (bend-verbs)

(defun other-state-change-verbs ()
  (let ((a '(abate advance age air alter atrophy awake balance blast blur bum burst capsize change char
                   chill clog close collapse collect compress condense contract corrode crumble decompose
                   decrease deflate defrost degrade diminish dissolve distend divide double drain ease
                   enlarge expand explode fade fill flood fray freeze frost fuse grow halt heal heat hush
                   ignite improve increase inflate kindle light loop mature melt multiply overturn pop
                   quadruple rekindle reopen reproduce rupture scorch sear short short-circuit shrink
                   shrivel singe sink soak splay sprout steep stretch submerge subside taper thaw tilt
                   tire topple triple unfold vary warp))
        (zero-related-to-adjective '(blunt clear clean cool crisp dim dirty double dry dull
                                          empty even firm level loose mellow muddy narrow open pale
                                          quiet round shut slack slim slow smooth sober sour steady
                                          tame tense thin triple warm))
        (color-change '(blacken brown crimson gray green purple redden silver tan whiten yellow))
        (en-verbs '(awaken brighten broaden cheapen coarsen dampen darken deepen fatten flatten freshen
                          gladden harden hasten heighten lengthen lessen lighten loosen moisten neaten
                          quicken quieten soften ripen steepen roughen stiffen sharpen straighten shorten
                          strengthen sicken slacken smarten sweeten tauten thicken tighten toughen
                          waken weaken widen worsen))
        (ify-verbs '(acetify acidify alkalify calcify carbonify dehumidify emulsify fructify
                            gasify humidify intensify lignify liquefy magnify nitrify ossifo/ petrify
                            purify putrefy silicify solidify stratify vitrify))
        (ize-verbs '(americanize caramelize carbonize crystallize decentralize demagnetize democratize
                                depressurize destabilize energize equalize fossilize gelatinize glutenize
                                harmonize hybridize iodize ionize magnetize neutralize oxidize polarize
                                pulverize regularize stabilize unionize vaporize volatilize westernize))
        (ate-verbs '(accelerate agglomerate ameliorate attenuate coagulate decelerate de-escalate degenerate
                               desiccate deteriorate detonate disintegrate dissipate evaporate federate
                               granulate incubate levitate macerate operate proliferate propagate ulcerate
                               vibrate)))
    (append a zero-related-to-adjective color-change en-verbs ify-verbs ize-verbs ate-verbs)))
;; (other-state-change-verbs)

(defun amuse-psych-verbs ()
  '(cheer delight enthuse gladden grieve madden obsess puzzle sadden sicken thrill tire weary worry))
;; (amuse-psych-verbs)

(defun change-of-possession-verbs ()
  (let ((give-verbs '(feed give lease lend loan pass pay peddle refund render rent repay sell serve trade))
        (contribute-verbs '(administer contribute disburse distribute donate extend forfeit proffer refer
                                      reimburse relinquish remit restore return sacrifice submit surrender transfer))
        (future-having '(advance allocate allot assign award bequeath cede concede extend grant guarantee issue leave
                                offer owe promise vote will yield)))
    (append give-verbs contribute-verbs future-having)))
;;(change-of-possession-verbs)

(defun cutting-verbs ()
  (let ((cut '(chip clip cut hack hew saw scrape scratch slash snip))
        (carve '(bore bruise carve chip chop crop crush cube
                     dent dice drill file fillet gash gouge grate grind
                     mangle mash mince mow nick notch perforate pulverize punch
                     prune shred slice slit spear squash squish)))
    (append cut carve)))
;;(cutting-verbs)

(defun contact-by-impact-verbs ()
  (let ((hit '(bang bash batter beat bump butt dash drum hammer hit kick knock lash punch pound
                   rap slap smack strike tamp tap thump thwack whack))
        (swat '(bite claw paw peck punch scratch shoot slug stab swat swipe))
        (spank '(belt birch bludgeon bonk brain cane clobber club conk cosh cudgel cuff flog
                     knife paddle paddywhack pummel sock spank strap thrash truncheon wallop whip whisk)))
    (append hit swat spank)))
;; (contact-by-impact-verbs)

(defun touch-verbs ()
  '(caress graze kiss lick nudge pat pinch prod sting stroke tickle touch))
;; (touch-verbs)

(defun destroy-verbs ()
  '(annihilate blitz decimate demolish destroy devastate exterminate extirpate obliterate
              ravage raze ruin waste wreck))
;; (destroy-verbs)

(defun killing-verbs ()
  '(assassinate butcher dispatch eliminate execute immolate kill liquidate massacre murder slaughter
               slay crucify electrocute garrotte hang knife poison shoot smother stab strangle))
;; (killing-verbs)

(defun appearance-verbs ()
  '(appear arise awake come dawn emanate emerge erupt evolve flow gush issue materialize plop
          result rise steal stem stream supervene surge wax))
;; (appearance-verbs)

(defun disappearance-verbs ()
  '(die disappear expire lapse perish vanish))
;; (disappearance-verbs)

(defun occurrence-verbs ()
  '(ensue eventuate happen occur recur transpire))
;; (occurrence-verbs)

(defun run-verbs () ;; Induced cause e.g. they ran the rats into the maze
  '(canter drive fly gallop jump leap march race run swim trot walk))
;; (run-verbs)

(defun emission-verbs (&key select)
  (let ((verbs `((sound (bang beep blare buzz clack clang clash clatter click hoot
                              jangle jingle ring rustle squeak squeal tinkle twang))
                 (light (beam blink flash shine))
                 (substance (belch bleed bubble dribble drip drool emanate exude gush
                                   leak ooze pour puff radiate seep shed spew spout sprout
                                   spurt squirt steam stream sweat)))))
    (or (second (assoc select verbs))
        (apply #'append (mapcar #'second verbs)))))
;; (emission-verbs)

(defun spatial-configuration-verbs ()
  '(dangle fly hang lean perch rest sit stand swing))
;; (spatial-configuration-verbs)

(defun lodge-verbs ()
  '(bivouac board lodge settle shelter))
;; (lodge-verbs)

(defun suffocate-verbs ()
  '(asphyxiate choke drown suffocate))
;; (suffocate-verbs)

;;; 1.1.3 Substance/Source alternation
;;; verbs of substance emission
;;; (emission-verbs :select 'substance)
;;;
;;; Heat radiates from the sun. / The sun radiates heat.

;;; ----------------------------------------------------------------------------
;;; 1.2 Unexpressed Object
;;; ----------------------------------------------------------------------------

;;; 1.2.1 Unspecified
(defun unexpressed-object-verbs ()
  '(bake carve chop clean cook crochet draw drink dust eat embroider hum hunt fish
         iron knead knit mend milk mow nurse pack paint play plow polish read recite
         sew sculpt sing sketch sow study sweep teach type sketch vacuum wash weave
         whittle write))
;; Mike ate the cake. / Mike ate.

;;; 1.2.2 Understood object (body-part)
;;; left out. p. 34

(defun hurt-verbs ()
  '(bark bite bump bum break bruise chip cut fracture
         rupture scald scratch skin split sprain
         strain stub tum twist hurt injure nick
         prick pull))
;; (hurt-verbs)

;;; 1.2.3 Understood reflexive



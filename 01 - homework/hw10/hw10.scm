; 1.

; (see "q1-im-client.scm")

; 2.

; (see "q2-im-client.scm" and "q2-im-server.scm")

; 3.

; (i) yes, we could have implemented single-source custom-multiple-target message-send on server side instead of on client side. but, because on server side single-source single-target message-send is already supported, implementing on client side means we only modify client. in contrast, implementing on server side means we have to have a new message supported that involves copying a target list for the server and then also we have to support that message on the client as well; in short, we have to then modify both client and server.

; (ii) yes, because the client is kept updated in terms of overall client list; broadcast then can be defined in terms of many single-source single-target message-send request. however, this gives the server less control over what can or can not be performed by a client, given that a broadcast is not recognized as a special request.

; 4.

; if we implement blocking at client side, it's easier to potentially bypass, as opposed to with having modification at server side; for example, we might inspect network traffic and modify target user accordingly and thereby be able to relatively straightforwardly flout the blocking.

; we want a refuse command issued from a client that the server acknowledges

; we want the send command to take into account refusing at the server side

; we want a successful/unsuccessful message issued from server that a client acknowledges

; have refuse, send, send-status

; (see "q4-im-client.scm" and "q4-im-server.scm")

; 5.

; three-way handshake is necessary because we have two steps, each in different directions, that each need a confirmation, turning two steps into three

; ex. 3.38

; peter attempts to deposit $10, paul attempts to withdraw $20, mary attempts to withdraw half of the money in the account

; a. 6 permutations if we have three task groups, with size of each being one - P1, P2, M:

; 3! / (1! * 1! * 1!) = 3! = 6

; P1 P2 M: 100 -> 110 -> 90 -> 45

; P1 M P2: 100 -> 110 -> 55 -> 35

; P2 P1 M: 100 -> 80 -> 90 -> 45

; P2 M P1: 100 -> 80 -> 40 -> 50

; M P1 P2: 100 -> 50 -> 60 -> 40

; M P2 P1: 100 -> 50 -> 30 -> 40

; b.

; P1: r1 w1

; P2: r2 w2

; M: r3 w3

; 90 permutations if we have three task groups, with size of each being two

; (3 * 2)! / (2! * 2! * 2!) = 720 / (2 ^ 3) = 720 / 8 = 90

; a few examples:

; (i) r2 r1 w2 w1 r3 w3 (equivalent to N/A P1 M): 100 -> 110 -> 55

; (ii) r3 r1 w3 w1 r2 w2 (equivalent to N/A P1 P2): 100 -> 110 -> 90

; (iii) r2 r3 w2 w3 r1 w1 (equivalent to N/A M P1): 100 -> 50 -> 60

; (iv) r1 r3 w1 w3 r2 w2 (equivalent to N/A M P2): 100 -> 50 -> 30

; (see "ex. 3.38 b - 1.png", "ex. 3.38 b - 2.png")

; ex. 3.39

; five unique outcomes become less

; we're steadily increasing the amount of detail that is involved in our race conditions; 
; here, we have up to two reads for a task group

; the serializer is s.t. we pass methods and get methods in return 
; that behave s.t. these result methods don't have their actions 
; non-completely interleaved with each other

; to recap:

; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (+ x 1))))

; BECOMES

; (define s (make-serializer))

; (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;                   (s (lambda () (set! x (+ x 1)))))

; the setting for first supplied method is separate from reading and
; the entirety of second supplied method is atomic; 
; essentially, we turned 2-read-1-write and 1-read-1-write into 
; 1-read-1-write and 1-task

; 5! / (3! * 2!) = 120 / (6 * 2) = 120 / 12 = 10

; 3! / (2! * 1!) = 3! / 2! = 6 / 2 = 3

; task group #1: r1 w1
; task group #2: a1

; r1 w1 a1: 10 -> 100 -> 101

; a1 r1 w1: 10 -> 11 -> 121

; r1 a1 w1: 10 -> 100

; the three outcomes out of five that survive are 100, 101, 121

; ex. 3.40

; turn 2-read-1-write and 3-read-1-write into 
; 1-task and 1-task

; 7! / (3! * 4!) = 5040 / (6 * 24) = 5040 / 144 = 35

; task group A: r_a1 r_a2 w_a1
; task group B: r_b1 r_b2 r_b3 w_b1

; 2! / (1! * 1!) = 2! = 2

; we determine the permutations via a program to interleave two strings

; (see "interleave.py")

; 35 permutations:

; interleaving "456" with "7890"

; 4 becomes r_a1, 
; 5 becomes r_a2, 
; 6 becomes w_a1, 
; 7 becomes r_b1, 
; 8 becomes r_b2, 
; 9 becomes r_b3, 
; 0 becomes w_b1

; r_a1 r_b1 r_b2 r_a2 w_a1 r_b3 w_b1: 10 -> 100 -> 10 * 10 * 100 = 10,000
; r_a1 r_b1 r_b2 r_b3 w_b1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 * 1,000 = 10,000
; r_a1 r_b1 r_b2 r_b3 r_a2 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_a1 r_b1 r_a2 r_b2 w_a1 r_b3 w_b1: 10 -> 100 -> 10 ^ 2 * 100 = 100 * 100 = 10,000
; r_b1 r_b2 r_a1 r_a2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_b2 r_b3 r_a1 r_a2 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_a1 r_b1 r_b2 r_b3 r_a2 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_a1 r_b1 r_a2 r_b2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_a1 r_a2 r_b2 r_b3 w_a1 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 3 = 1,000
; r_b1 r_a1 r_b2 r_b3 r_a2 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_b2 r_b3 r_a1 r_a2 w_a1 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 3 = 1,000
; r_b1 r_a1 r_b2 r_a2 r_b3 w_a1 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 3 = 1,000
; r_a1 r_a2 r_b1 r_b2 w_a1 r_b3 w_b1: 10 -> 100 -> 10 ^ 2 * 100 = 100 * 100 = 10,000
; r_a1 r_b1 r_a2 r_b2 r_b3 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_b1 r_b2 r_a1 r_b3 w_b1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 * 1,000 = 10,000
; r_b1 r_b2 r_b3 r_a1 w_b1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 * 1,000 = 10,000
; r_b1 r_a1 r_b2 r_b3 r_a2 w_a1 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 3 = 1,000
; r_b1 r_a1 r_a2 r_b2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_a1 r_a2 r_b2 w_a1 r_b3 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 2 * 100 = 100 * 100 = 10,000
; r_b1 r_b2 r_b3 w_b1 r_a1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 1,000 * 1,000 = 1,000,000
; r_b1 r_a1 r_b2 r_a2 w_a1 r_b3 w_b1: 10 -> 10 ^ 2 = 100 -> 10 ^ 2 * 100 = 100 * 100 = 10,000
; r_b1 r_b2 r_a1 r_b3 r_a2 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_a1 r_b2 r_a2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_a1 r_a2 r_b1 w_a1 r_b2 r_b3 w_b1: 10 -> 10 ^ 2 = 100 -> 10 * 100 ^ 2 = 10 * 10,000 = 100,000
; r_b1 r_a1 r_a2 w_a1 r_b2 r_b3 w_b1: 10 -> 10 ^ 2 = 100 -> 10 * 100 ^ 2 = 10 * 10,000 = 100,000
; r_a1 r_a2 w_a1 r_b1 r_b2 r_b3 w_b1: 10 -> 100 -> 100 ^ 3 = 1,000,000
; r_b1 r_b2 r_a1 r_a2 r_b3 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_b1 r_b2 r_a1 r_b3 r_a2 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_a1 r_a2 r_b1 r_b2 r_b3 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_b1 r_a1 r_b2 r_b3 w_b1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 * 1,000 = 10,000
; r_a1 r_b1 r_b2 r_a2 r_b3 w_a1 w_b1: 10 -> 100 -> 10 ^ 3 = 1,000
; r_a1 r_b1 r_a2 w_a1 r_b2 r_b3 w_b1: 10 -> 100 -> 10 * 100 ^ 2 = 10 * 10,000 = 100,000
; r_a1 r_a2 r_b1 r_b2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_a1 r_b1 r_b2 r_a2 r_b3 w_b1 w_a1: 10 -> 10 ^ 3 = 1,000 -> 10 ^ 2 = 100
; r_b1 r_b2 r_a1 r_a2 w_a1 r_b3 w_b1: 10 -> 100 -> 10 ^ 2 * 100 = 100 * 100 = 10,000

; unique outcomes are: 100, 1,000, 10,000, 100,000, 1,000,000

; r_a1 r_a2 w_a1 r_b1 r_b2 r_b3 w_b1: 10 -> 100 -> 100 ^ 3 = 1,000,000
; r_b1 r_b2 r_b3 w_b1 r_a1 r_a2 w_a1: 10 -> 10 ^ 3 = 1,000 -> 1,000 * 1,000 = 1,000,000

; unique outcomes are: 1,000,000

; ex. 3.41

; ben bitdiddle is right, though his approach is technically overkill

; the approach is overkill because we can view withdraw and deposit as writes and balance retrieval as a read; then, we view the problem as an instance of readers-writers, which functions by having writes be atomic w.r.t. reads and other writes and by having reads be non-atomic w.r.t. other reads; the approach he suggests satisfies these changes and adds on top of them having reads be atomic w.r.t. other reads

; still, the question does not ask about whether the approach is optimal; so, a specific example where we improve is the following:

; as a guide, the important part that affects correctness here is that write is atomic w.r.t. read

; (here, the write is deposit and read is balance retrieval)

; (assume that we have a joint account that begins with a balance of $100)

; person a: DEPOSIT $20 (w_a1), RETRIEVE-BALANCE (r_a1)

; person b: RETRIEVE-BALANCE (r_b1), DEPOSIT half of read balance (w_b1)

; before change: {w_a1, r_a1, r_b1}, {w_b1} => r_b1, w_a1, r_a1, w_b1 => 100 -> 120 -> 120 -> 70

;	OR: w_a1, r_a1, r_b1, w_b1 => 100 -> 120 -> 120 -> 60

; after change: {w_a1}, {r_a1}, {r_b1}, {w_b1} => w_a1, r_a1, r_b1, w_b1 => 100 -> 120 -> 120 -> 60

; (note that having two possible outcomes for this specific "equivalence class" sequence for the pre-change account implementation shows that we were sometimes inconsistent and therefore were wrong overall, as opposed to having one possible outcome for this sequence for the post-change account implementation)



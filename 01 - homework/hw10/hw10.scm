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

; a. 

; b.



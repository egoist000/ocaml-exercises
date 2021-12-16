(*
 * Dal compito d’esame di giugno 2010) Le cassaforti della marca VeryHard hanno
 * un sistema di apertura alquanto singolare: ciascuna di esse ha un numero N di
 * chiavi, disposte in sequenza, ciascuna delle quali può essere in posizione aperta
 * o chiusa. Solo quando tutte le chiavi sono aperte, la cassaforte si apre. Le
 * chiavi devono essere girate una alla volta (passando così dalla posizione aperta
 * a chiusa o viceversa), ma data una certa configurazione delle chiavi, non è
 * possibile girare una chiave qualsiasi, ma soltanto la prima chiave (iniziando
 * da sinistra), oppure la chiave che segue immediatamente la prima chi-
 * ave chiusa (iniziando sempre da sinistra). Ad esempio, data la configurazione
 * di chiavi seguente (dove C indica che la chiave è chiusa, A che è aperta):
 * A A A C C A C è possibile girare la prima chiave, passando alla configurazione
 * C A A C C A C, oppure la quinta (infatti la prima chiave chiusa è la quarta),
 * passando a A A A C A A C. Se la prima chiave chiusa della configurazione è
 * l’ultima (come ad esempio in A A C), si può soltanto girare la prima chiave.
 *)

(* Date le seguenti dichiarazioni di tipo *)

type chiave = Aperta | Chiusa 
type cassaforte = chiave list

exception NoKey

(* gira_prima: cassaforte -> cassaforte *)
(* gira_prima c = riporta la configurazione della cassaforte che si ottiene
 * girando la prima chiave *)

let rec gira_prima = function
    | [] -> []
    | x::rest ->
            match x with
            | Aperta -> Chiusa::rest
            | Chiusa -> Aperta::rest

(* gira_dopo_chiusa: cassaforte -> cassaforte *)
(* gira_dopo_chiusa c = la cassaforte che si ottiene girando la 
 * chiave dopo la prima chiave chiusa *)

let rec gira_dopo_chiusa = function
    | [] -> raise NoKey
    | x::[] -> raise NoKey
    | x::rest ->
            match x with
            | Aperta -> x::gira_dopo_chiusa rest
            | Chiusa -> x::gira_prima rest

(* successori: cassaforte -> cassaforte list *)
(* successori clist = una lista contenente la configurazione della cassaforte 
 * dopo l'applicazione di gira_prima e gira_dopo_chiusa, se gira_dopo_chiusa 
 * non è applicabile la lista conterrà un solo elemento *)

let successori clist =
    (gira_prima clist)::
        (try [gira_dopo_chiusa clist] with
        | NoKey -> [] )

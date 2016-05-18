
Csili ist eine funktionale und nebenläufige Programmiersprache, in der funktionaler Kern und Kontrollfluss voneinander getrennt sind.
Das Hauptziel von Csili ist, Nebenläufigkeit auszunutzen.
Da sich Nebenläufigkeit hervorragend mit Petrinetzen beschreiben lässt und Petrinetze bereits jahrzehntelang erforscht wurden, modellieren wir den Kontrollfluss als Petrinetz.
D.h., der Csili-Compiler erzeugt aus einem Csili-Programm einen Scheduler, der das modellierte Petrinetz ausführt.
Um sicherzustellen, dass das Feuern einer Transition ein atomarer Schritt ist, ist der funktionale Kern von Csili total.

Ein weiterer wesentlicher Aspekt ist die Optimierung eine Csili-Programms.

Auf der untersten Ebene befinden sich Ausdrücke, denen ein Typ zugeordnet ist.
Die nächsthöhere Ebene beinhaltet Funktionen, die aus Ausdrücken neue Ausdrücke erzeugen, und Muster, die man mit Ausdrücken abgleichen kann.
Diese Elemente werden im Kontrollfluss wiederverwendet:
Jedem Platz ist ein Typ zugeordnet und auf einem Platz dürfen sich nur Ausdrücke desselben Typs befinden.
Jede Kante zwischen einer Transition t und einem Vorplatz von t ist mit einem Muster beschriftet;
abweichend von der gängigen Petrinetzsemantik ist t erst aktiviert, wenn für alle Plätze p im Vorbereich von t gilt, dass das Muster der Kante (p,t) mit 
Jede Kante zwischen t und einem Nachplatz von t ist mit einem Ausdruck beschriftet,

TODO: Den gewählten Formalismus mit gefärbten Petrinetzen vergleichen.
TODO: Was ist der Unterschied zwischen Kontroll- und Datenfluss?
TODO: Totale funktionale Programmiersprachen erforschen (z.B. Coq).

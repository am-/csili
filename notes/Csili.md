
Csili ist eine funktionale und nebenläufige Programmiersprache, in der funktionaler Kern und Kontrollfluss voneinander getrennt sind.
Das Hauptziel von Csili ist, Nebenläufigkeit auszunutzen.
Da sich Nebenläufigkeit hervorragend mit Petrinetzen beschreiben lässt und Petrinetze bereits jahrzehntelang erforscht wurden, modellieren wir den Kontrollfluss als Petrinetz.
D.h., der Csili-Compiler erzeugt aus einem Csili-Programm einen Scheduler, der das modellierte Petrinetz ausführt.
Um sicherzustellen, dass das Feuern einer Transition ein atomarer Schritt ist, ist der funktionale Kern von Csili total.

Der funktionale Kern wird in ein Termersetzungssystem übersetzt.

Ein weiterer wesentlicher Aspekt ist die Optimierung eine Csili-Programms.

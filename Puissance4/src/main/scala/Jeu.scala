import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Jeu extends App {

  type Plateau = Array[Array[Int]]
  type Ligne = Array[Int]

  def afficherChiffre(nbColonne : Int) = {
    print("| ")
    for(i <- 0 until nbColonne){
      print(i + " | ")
    }
    println()
  }

  def afficherElement(elt : Int) = elt match {
    case 0 => print(" ")
    case 1 => print("R")
    case 2 => print("J")
  }

  def afficherLigne(ligne : Ligne) = {
    print("| ")
    for(elt <- ligne){
      afficherElement(elt)
      print(" | ")
    }
    print("\n+")
    for(elt <- ligne){
      print("---+")
    }
    println
  }

  def afficherPlateau(plateau : Plateau) = {
    println()
    for(ligne <- plateau){
      afficherLigne(ligne)
    }
    afficherChiffre(plateau(0).length)
    println()
  }

  def chercherMaxConsecutif(plateau : Plateau) : Int = {
    var c : Int = chercherTroisConsecutif(plateau)
    c match {
      case -1 => obtenirCoupAleatoire(plateau) 
      case _ => c
    }
  }

  def chercherTroisConsecutif(plateau : Plateau) : Int = {
    var c : Int = -1
    for(i <- 0 until plateau(0).length){
      var bool = false
      var ligne = parcourirColonneJoue(plateau, i, plateau.length - 1)
      if(ligne != -1) {
        bool = bool || parcourirColonneDroite3(plateau, ligne, i + 1, 1, 0)
        bool = bool || parcourirColonneDroite3(plateau, ligne, i + 1, 2, 0)
        bool = bool || parcourirColonneGauche3(plateau, ligne, i - 1, 1, 0)
        bool = bool || parcourirColonneGauche3(plateau, ligne, i - 1, 2, 0)
        bool = bool || parcourirLigne3(plateau, ligne + 1, i, 1, 0)
        bool = bool || parcourirLigne3(plateau, ligne + 1, i, 2, 0)
        bool = bool || parcourirDiagonaleHG(plateau, ligne - 1, i - 1, 1, 0)
        bool = bool || parcourirDiagonaleHG(plateau, ligne - 1, i - 1, 2, 0)
        bool = bool || parcourirDiagonaleBG(plateau, ligne + 1, i - 1, 1, 0)
        bool = bool || parcourirDiagonaleBG(plateau, ligne + 1, i - 1, 2, 0)
        bool = bool || parcourirDiagonaleBD(plateau, ligne + 1, i + 1, 1, 0)
        bool = bool || parcourirDiagonaleBD(plateau, ligne + 1, i + 1, 2, 0)
        bool = bool || parcourirDiagonaleHD(plateau, ligne - 1, i + 1, 1, 0)
        bool = bool || parcourirDiagonaleHD(plateau, ligne - 1, i + 1, 2, 0)
      }
      if(bool){
        c = i
      }     
    }
    c
  }

  def demanderChaine() : String = {
    println("Donner chaine format ligne\\nligne\\n... : ")
    val s = readLine
    s
  }

  def demanderColonneCoupHumain(plateau : Plateau) : Int = {
    println("Choisissez un chiffre entre 0 et " + (plateau(0).length - 1) + " jouable : ")
    val i = readInt
    plateau(0)(i) match {
      case 0 => i
      case _ => demanderColonneCoupHumain(plateau)
    }
  }

  def demanderCouleurJoueur() : Int = {
    println("Choisir entre rouge et jaune : ")
    val s = readLine
    s match {
      case "Rouge" | "rouge" | "R" | "r" => 1
      case "Jaune" | "jaune" | "J" | "j" => 2
      case _ => demanderCouleurJoueur()
    }
  }

  def demanderInitialisation() : Int = {
    println("Choisir entre 1 - importer ou 2 - nouveau : ")
    val s = readLine
    s match {
      case "importer" | "1" | "1 - importer" => 1
      case "nouveau" | "2" | "2 - nouveau"=> 2
      case _ => demanderInitialisation()
    }
  }

  def demanderTailleTableau(s : String) : Int = {
    println("Donner " + s + " entre 4 et 10 : ")
    val i = readInt
    i match {
      case 4 | 5 | 6 | 7 | 8 | 9 | 10 => i
      case _ => demanderTailleTableau(s)
    }
  }

  def demanderTypeJeu() : Plateau = {
    var i : Int = demanderInitialisation()
    i match {
      case 1 =>
        { var s = demanderChaine()
          var p = importPlateau(s)
          p
        }
      case 2 =>
        {
          var ligne : Int = demanderTailleTableau("longueur")
          var colonne : Int = demanderTailleTableau("largeur")
          var p : Plateau = Array.ofDim(ligne, colonne)
          initialiserPlateau(p, ligne, colonne)
          p
        }
    }
  }

  def exportLigne(ligne : Ligne) : String = {
    var s : String = ""
    for(elt <- ligne){
      s = s + (obtenirCouleur(elt).charAt(0))
    }
    s += '\n'
    s
  }

  def exportPlateau(plateau : Plateau) : String = {
    var s : String = ""
    for(ligne <- plateau){
      s = s + exportLigne(ligne)
    }
    s
  }

  def importElement(elt : Char) : Int = elt match {
    case 'R' => 1
    case 'J' => 2
    case _ => 0
  }

  def importPlateau(chaine : String) : Plateau = {
    var tableauChaine = chaine.split("\n")
    var ligne = tableauChaine.length
    var colonne = tableauChaine(0).length
    var plateau : Plateau = Array.ofDim(ligne, colonne)
    for(i <- 0 until ligne; j <- 0 until colonne){
      plateau(i)(j) = importElement(tableauChaine(i)(j))
    }
    plateau
  }

  def initialiserJeu() = { 
    var plateau : Plateau = demanderTypeJeu()
    var couleurHumain : Int = demanderCouleurJoueur()
    var couleurOrdi : Int = obtenirCouleurInverse(couleurHumain)
    var finJeu : Boolean = false
    afficherPlateau(plateau)
    jouer(plateau, obtenirCouleurCommence(), couleurHumain)
  }

  def initialiserPlateau(plateau : Plateau, ligne : Int, colonne : Int) = {
    for(i <- 0 until ligne; j <- 0 until colonne){
      plateau(i)(j) = 0
    }
  }

  def joueCoupHumain(plateau : Plateau, colonne : Int, couleurHumain : Int) = {
    plateau(parcourirColonneJoue(plateau, colonne, plateau.length -1))(colonne) = couleurHumain
  }

  def joueCoupOrdi(plateau : Plateau, couleurOrdi : Int) = {
    println("C'est au tour du joueur " + obtenirCouleur(couleurOrdi))
    val colonne : Int = chercherMaxConsecutif(plateau)//obtenirCoupAleatoire(plateau)
    plateau(parcourirColonneJoue(plateau, colonne, plateau.length -1))(colonne) = couleurOrdi
  }

  def joueTourCoupHumain(plateau : Plateau, couleurHumain : Int) = {
    println("C'est au tour du joueur " + obtenirCouleur(couleurHumain))
    joueCoupHumain(plateau, demanderColonneCoupHumain(plateau), couleurHumain)    
  }

  def jouer(plateau : Plateau, couleurJoueur : Int, couleurHumain : Int) : Boolean = {
    couleurJoueur match {
      case `couleurHumain` => joueTourCoupHumain(plateau, couleurJoueur)
      case _ => joueCoupOrdi(plateau, couleurJoueur)
    }
    afficherPlateau(plateau)
    parcourirPlateauGagne(plateau, couleurJoueur) match {
      case false => jouer(plateau,obtenirCouleurInverse(couleurJoueur), couleurHumain)
      case true => {
        parcourirLigneJoue(plateau, 0, 0) match {
        case true => println("Match nul")
        case _ => println("La joueur " + couleurJoueur + " Gagne")}}
    }
    false
  }

  def obtenirCouleur(couleur : Int) : String = couleur match {
    case 1 => "Rouge"
    case 2 => "Jaune"
    case 0 => " "
  }

  def obtenirCouleurCommence() : Int = {
    val rand = new scala.util.Random
    rand.nextInt(2) + 1
  }

  def obtenirCouleurInverse(couleur : Int) : Int = couleur match {
    case 1 => 2 
    case 2 => 1 
  }

  def obtenirCoupAleatoire(plateau : Plateau) : Int = {
    val rand = new scala.util.Random
    val i = rand.nextInt(plateau(0).length)
    plateau(0)(i) match {
      case 0 => i
      case _ => obtenirCoupAleatoire(plateau)
    }
  }

  def parcourirColonneDroite3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbColonne : Int = plateau(0).length
    colonne match{
      case `nbColonne` => i == 3
      case _ => verifieColonneDroite3(plateau, ligne, colonne, couleurJoueur, i)
    } 
  }

  def parcourirColonneGagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbColonne : Int = plateau(0).length 
    colonne match{
      case `nbColonne` => i == 4
      case _ => verifieColonneGagne(plateau, ligne, colonne, couleurJoueur, i)
    } 
  }

  def parcourirColonneGauche3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbColonne : Int = -1
    colonne match{
      case `nbColonne` => i == 3
      case _ => verifieColonneGauche3(plateau, ligne, colonne, couleurJoueur, i)
    } 
  }

  def parcourirColonneJoue(plateau : Plateau, colonne : Int, i : Int) : Int = plateau(i)(colonne) match {
    case 0 => i
    case _ => {
      i match{
        case 0 => -1
        case _ => parcourirColonneJoue(plateau, colonne, i - 1)
      }
    }
  }

  def parcourirDiagonaleBD(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau.length
    val nbColonne : Int = plateau(0).length
    ligne match{
      case `nbLigne` => i == 3
      case _ => {colonne match {
        case `nbColonne` => i == 3
        case _ => verifieDiagonaleBD(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirDiagonaleBG(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau(0).length 
    ligne match{
      case `nbLigne` => i == 3
      case _ => {colonne match {
        case -1 => i == 3
        case _ => verifieDiagonaleBG(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirDiagonale1Gagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau.length 
    val nbColonne : Int = plateau(0).length 
    ligne match{
      case `nbLigne` => i == 4
      case _ => {colonne match {
        case `nbColonne` => i == 4
        case _ => verifieDiagonale1Gagne(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirDiagonale2Gagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau.length  
    ligne match{
      case `nbLigne` => i == 4
      case _ => {colonne match {
        case 0 => i == 4
        case _ => verifieDiagonale2Gagne(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirDiagonaleHD(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbColonne : Int = plateau(0).length
    ligne match{
      case -1 => i == 3
      case _ => {colonne match {
        case `nbColonne` => i == 3
        case _ => verifieDiagonaleHD(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirDiagonaleHG(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = -1
    ligne match{
      case `nbLigne` => i == 3
      case _ => {colonne match {
        case -1 => i == 3
        case _ => verifieDiagonaleHG(plateau, ligne, colonne, couleurJoueur, i)
      }}
    } 
  }

  def parcourirJetonGagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, finJeu : Boolean) : Boolean = {
    finJeu || parcourirLigneJoue(plateau, 0, 0) || parcourirColonneGagne(plateau, ligne, colonne, couleurJoueur, 0) || parcourirLigneGagne(plateau, ligne, colonne, couleurJoueur, 0) || parcourirDiagonale1Gagne(plateau, ligne, colonne, couleurJoueur, 0) || parcourirDiagonale2Gagne(plateau, ligne, colonne, couleurJoueur, 0)
  }

  def parcourirLigne3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau.length
    ligne match{
      case `nbLigne` => i == 3
      case _ => verifieLigne3(plateau, ligne, colonne, couleurJoueur, i)
    } 
  }

  def parcourirLigneGagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = {
    val nbLigne : Int = plateau.length 
    ligne match{
      case `nbLigne` => i == 4
      case _ => verifieLigneGagne(plateau, ligne, colonne, couleurJoueur, i)
    } 
  }

  def parcourirLigneJoue(plateau : Plateau, ligne : Int, i : Int) : Boolean = plateau(ligne)(i) match {
    case 0 => false
    case _ => {
      val nbColonne = plateau(0).length
      ligne match {
        case `nbColonne` => true
        case _ => parcourirLigneJoue(plateau, ligne, i + 1)
      }
    }
  }

  def parcourirPlateauGagne(plateau : Plateau, couleurJoueur : Int) : Boolean = {
    var finJeu = false
    val joueur : Int = couleurJoueur
    for(i <- 0 until plateau.length; j <- 0 until plateau(0).length){
      plateau(i)(j) match {
        case joueur => {finJeu = parcourirJetonGagne(plateau, i, j, couleurJoueur, finJeu)}
      }      
    }
    finJeu
  }

  def verifieColonneDroite3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirColonneDroite3(plateau, ligne, colonne + 1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieColonneGagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirColonneGagne(plateau, ligne, colonne + 1, couleurJoueur, i + 1)
    case _ => {i == 4}
  }

  def verifieColonneGauche3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirColonneGauche3(plateau, ligne, colonne - 1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieDiagonaleBD(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonaleBD(plateau, ligne + 1, colonne + 1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieDiagonaleBG(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonaleBG(plateau, ligne + 1, colonne -1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieDiagonale1Gagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonale1Gagne(plateau, ligne + 1, colonne + 1, couleurJoueur, i + 1)
    case _ => {i == 4}
  }

  def verifieDiagonale2Gagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonale2Gagne(plateau, ligne + 1, colonne -1, couleurJoueur, i + 1)
    case _ => {i == 4}
  }

  def verifieDiagonaleHD(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonaleHD(plateau, ligne - 1, colonne + 1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieDiagonaleHG(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirDiagonaleHG(plateau, ligne - 1, colonne -1, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieLigne3(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirLigne3(plateau, ligne + 1, colonne, couleurJoueur, i + 1)
    case _ => {i == 3}
  }

  def verifieLigneGagne(plateau : Plateau, ligne : Int, colonne : Int, couleurJoueur : Int, i : Int) : Boolean = plateau(ligne)(colonne) match {
    case `couleurJoueur` => parcourirLigneGagne(plateau, ligne + 1, colonne, couleurJoueur, i + 1)
    case _ => {i == 4}
  }
  
  initialiserJeu()
  
}
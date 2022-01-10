#load "graphics.cma";;
open Graphics;;
open Random;;


open_graph("1400x800");;

type coords = {mutable x: float;mutable y: float};;
type vecteur = {mutable vx:float;mutable vy:float};;
type nature = Player | Spikes | ScoreBall | DoubleScoreBall| VectorBall | RandomBall | EnergyBall | NoEnergyBall | Plateforme;; 
type gameState = Menu | Idle | Aiming ;; 


type color = {mutable r: float;mutable g: float, mutable b: float};;
let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;

let generate_cord = 
  let min, max = map in
    {x = float max.x ;
    y = float max.y };;


let collision entite1 entite2 = 
	let dist = sqrt( (entite1#getPos.x*entite2#getPos.x) +. (entite1#getPos.y*entite2#getPos.y) ) in
    if dist <= entite1#getRayon+.entite2#getRayon then true else false;;

(*à utiliser pour mesurer distance Souris Ball*)
let distance (x1, y1) (x2, y2) = 
	let dx = x1- x2 and dy = y1-y2 in
		dx * dx  + dy * dy;;

class entity id pos color nature rayon= 
	object(self)
	 val id = (id: int);
    val nature = (nature: nature);
    val mutable pos = (pos : coords);
    val mutable speed = ({ vx = 0.; vy = 0.;} : vecteur);
    val mutable color = (color : color);
    
    method getRayon = 8.
    method getPos = pos;
    method getColor = color;
    method getId = id;
    method getNature = nature;
    method getSpeed = speed;
    
    end
	;;



let bg = rgb 255 255 255 and color_spike= rgb 34 177 76 and color_player= rgb 0 64 128 and color_scoreBall = rgb 210 0 0 and color_doubleScoreBall= rgb 255 0 0 and color_vectorBall = rgb 255 201 14 and color_vector = rgb 0 0 0 and color_noEnergyBall= rgb 128 0 128;;

let cree_vecteur point1 point2 =
	{vx = point2.x -. point1.x; vy = point2.y -. point1.y};;

let construit_entiter pos vit nat nature r =
    let rayon = ref r in {
    		position = pos;
         vitesse = vit;
         nature = nat;
        	joueur = nature;
         set_rayon = (fun r -> rayon := r);
         get_rayon = (fun() -> !rayon);
};;

let attribution entity = 
let alea = int(50) in 
if alea > 45

let genNature () = let v = Random.int  in if v=1 then Matiere else Antimatiere;;

let collision entite1 entite2 = 
	let dist = sqrt( (entite1.position.x*entite2.position.x) +. (entite1.position.y*entite2.position.y) ) in
    if dist <= entite1.get_rayon()+.entite2.get_rayon() then true else false;;
 
let generate world nEntiteAGenerer rayonMax = 
    for i=0 to n-List.length entites do
        let ray = rayonMax/.10 +. Random.float (rayonMax -. rayonMax/.10) in
        let coo = {x=Random.float (1-ray); x=Random.float (1-ray) } in
        let newEntity = ref (construit_entiter coo 0 (genNature ()) (if i=1 then 1 else 0) ray) in
        	while List.exists (collision !newEntity entites) do
            	let ray = rayonMax/.10 +. Random.float (rayonMax -. rayonMax/.10) in
        		let coo = {x=Random.float 1-ray; x=Random.float 1-ray } in
                newEntity := construit_entiter(coo 0 (genNature) (if i=1 then 1 else 0) ray)
            done;
    done; 
;;


let open_window () = 
  open_graph "1400x800";
  set_window_title "Balls?";
  display_mode false;
  auto_synchronize false;;
open_window();;

clear_graph ();
set_color (rgb 20 20 20);
fill_rect 0 0 1400 800;
set_color (rgb 224 73 55);

draw_rect 590 345 200 40;
draw_rect 590 295 200 40; 
draw_rect 590 245 200 40; 


(*fills the rectangle with lower left corner at x,y, width w and heigth h, with the current color.*)
Graphics.moveto 550 450 ;
set_text_size 90;
Graphics.draw_string  "BALLS" ;
set_text_size 30;

set_color (rgb 245 194 188);
Graphics.moveto 645 350 ;
Graphics.draw_string "START" ;
Graphics.moveto 630 300 ;
Graphics.draw_string "CREDITS" ;
Graphics.moveto 645 250 ;
Graphics.draw_string "LEAVE" ;;
read_key;;

let rec event_loop time = 
  clear_graph();
  let newTime = Unix.gettimeofday () and (mousex, mousey) = mouse_pos () in
  let deltaTime = newTime -. time in

  List.iter (fun entity -> entity#updateState deltaTime player) player#getEntities;

  player#updatePlayerSpeed mousex mousey deltaTime;
  player#updateCoords deltaTime;

  (if key_pressed () then let key = read_key () in 
    match key with 
    | 'x' -> player#split;
    | _ -> ();
  );$
  
  (* MONITORING *)
  moveto 5 (size_y () - 15);
  set_color black;

  let playerCord = player#getMainEntity#getCoordonees in
  moveto 5 (size_y () - 30); draw_string (Printf.sprintf "Position : %0.0f, %0.0f" playerCord.x  playerCord.y);

  (match serverData with
    | None -> ()
    | Some _  -> moveto 5 (size_y () - 45);
      draw_string (Printf.sprintf "Connected : %d " (List.length (!other_players) + 1)));

  (* DRAWING *)
  player#destroyFoods;
  player#handleFoodCollisions;
  player#generateFoods;

  drawGrid player;

  drawEntities player player#getFoods (fun x y color radius -> (set_color color; fill_circle x y radius));

  drawEntities player (List.filter (fun entity -> entity#getState <> MainEntity) player#getEntities) (fun x y color radius -> (set_color color; fill_circle x y radius));
  drawMainPlayer player;
  
  drawEntities player !bushes (fun x y _color radius -> drawBush x y (float_of_int radius) "green");
  drawEntities player !bushesFood (fun x y _color radius -> drawBush x y (float_of_int radius) "red"); 
  (*spawn_buisson_feed player (List.hd (!bushesFood)); *)

  (match serverData with 
    | None -> ();
    | Some (_) -> List.iter (fun otherPlayer -> otherPlayer#updateCoords deltaTime; drawEntities player (otherPlayer#getEntities) (fun x y color radius -> set_color color; fill_circle x y radius)) (!other_players));

  synchronize ();
  event_loop newTime serverData;;
  
  

(*
let jeu () =  
	open_graph "";
	let r = ref 15 in
		while true do
			let a,b = alea_dans_fenetre !r in
				clear_graph ();
				fill_circle a b !r ;
				let clic = wait_next_event [Button_up] in
				let deplacement = cree_vecteur (a,b) (clic.mouse_x, clic.mouse_y) ;
					
		done;;
		
jeu ();;*)
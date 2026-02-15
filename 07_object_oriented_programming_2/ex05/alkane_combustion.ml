class alkane_combustion (alkanes : Molecule.alkane list) =
  object (self)
    inherit Reaction.reaction

    method private totals =
      List.fold_left (fun (c, h) a -> (c + a#carbon_count, h + a#hydrogen_count)) (0, 0) alkanes

    method private complete =
      let c, h = self#totals in
      let h2o = h / 2 in
      let o2 = (2 * c + h2o) / 2 in
      (c, h2o, o2)

    method is_balanced = true

    method get_start =
      let _, _, o2 = self#complete in
      (List.map (fun a -> ((a :> Molecule.molecule), 1)) alkanes) @ [ ((new Molecule.oxygen :> Molecule.molecule), o2) ]

    method get_result =
      let co2, h2o, _ = self#complete in
      [ ((new Molecule.carbon_dioxide :> Molecule.molecule), co2); ((new Molecule.water :> Molecule.molecule), h2o) ]

    method balance = (self :> Reaction.reaction)

    method get_incomplete_results : (int * (Molecule.molecule * int) list) list =
      let c, h = self#totals in
      let h2o = h / 2 in
      let rec build co2_left co acc =
        if co2_left < 0 then acc
        else
          let c_soot = c - co2_left - co in
          if c_soot < 0 then acc
          else
            let oxygen_atoms = (2 * co2_left) + co + h2o in
            if oxygen_atoms mod 2 <> 0 then build (co2_left - 1) co acc
            else
              let o2 = oxygen_atoms / 2 in
              let products =
                [ ((new Molecule.carbon_dioxide :> Molecule.molecule), co2_left); ((new Molecule.carbon_monoxide :> Molecule.molecule), co); ((new Molecule.carbon :> Molecule.molecule), c_soot); ((new Molecule.water :> Molecule.molecule), h2o) ]
                |> List.filter (fun (_, n) -> n > 0)
              in
              build (co2_left - 1) (co + 1) ((o2, products) :: acc)
      in
      build c 1 []
  end

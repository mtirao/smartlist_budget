//
//  BasketUpdateDTO.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 08/11/2024.
//

import Foundation
import Fluent
import Vapor

struct BasketDescriptionUpdateDTO: Content {
    let id: UUID
    let price: Double
    let lon: Double
    let lat: Double
    
    init(id: UUID, price: Double, lon: Double, lat: Double) {
        self.id = id
        self.price = price
        self.lon = lon
        self.lat = lat
    }
}

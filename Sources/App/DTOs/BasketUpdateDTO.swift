//
//  BasketUpdateDTO.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 08/11/2024.
//

import Foundation
import Fluent
import Vapor

struct BasketUpdateDTO: Content {
    let id: UUID
    let tender: UUID
    
    init(id: UUID, tender: UUID) {
        self.id = id
        self.tender = tender
    }
}

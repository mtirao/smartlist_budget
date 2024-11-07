//
//  TenderDTO.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Foundation
import Fluent
import Vapor

struct BasketDTO: Content {

    let id: UUID?
    var date: Date?
    var status: Status?
    var tenderId: UUID?
    
    init( id: UUID? = nil, date: Date? = nil, status: Status? = nil, tenderId: UUID? = nil) {
        self.id = id
        self.date = date
        self.status = status
        self.tenderId = tenderId
    }
    
    func toModel(userId: String) -> Basket {
        let model = Basket()
        
        model.id = self.id
        if let date = self.date {
            model.date = date
        }
        
        if let status = self.status {
            model.status = status
        }
        
        if let tenderId = self.tenderId {
            model.tenderId = tenderId
        }
        
        model.userId = userId

        return model
    }
}

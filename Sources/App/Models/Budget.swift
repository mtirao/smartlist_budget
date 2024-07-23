//
//  Budget.swift
//  
//
//  Created by Marcos Tirao on 23/07/2024.
//

import Fluent
import Foundation

final class Budget: Model, @unchecked Sendable {
    static let schema = "budget"
    
    @ID(key: .id)
    var id: UUID?

    @Field(key: "amount")
    var amount: Double
    
    @Field(key: "date")
    var date: Date
    
    @Field(key: "name")
    var name: String
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, name: String, date: Date, amount: Double, userId: String) {
        self.id = id
        self.name = name
        self.date = date
        self.amount = amount
        self.userId = userId
    }
    
    func toDTO() -> BudgetDTO {
        .init(
            id: self.id,
            name: self.$name.value,
            amount: self.$amount.value, 
            date: self.$date.value,
            user_id: self.$userId.value
        )
    }
}


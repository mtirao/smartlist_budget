//
//  Tender.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Foundation


final class Tender: Model, @unchecked Sendable {
    static let schema = "tenders"
    
    @ID(key: .id)
    var id: UUID?
    
    @Field(key: "type")
    var type: String
    
    @Field(key: "number")
    var number: String
    
    @Field(key: "alias")
    var alias: String
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, type: String, number: String, alias: String, userId: String) {
        self.id = id
        self.type = type
        self.number = number
        self.alias = alias
        self.userId = userId
    }
    
    func toDTO() -> TenderDTO {
        .init(
            id: self.id,
            type: self.$type.value,
            number: self.$number.value,
            alias: self.$alias.value
        )
    }
}

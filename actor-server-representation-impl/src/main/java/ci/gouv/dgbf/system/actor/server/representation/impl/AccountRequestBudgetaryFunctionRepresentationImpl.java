package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AccountRequestBudgetaryFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestBudgetaryFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AccountRequestBudgetaryFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<AccountRequestBudgetaryFunctionDto> implements AccountRequestBudgetaryFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

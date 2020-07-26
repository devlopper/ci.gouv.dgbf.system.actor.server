package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AccountRequestFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AccountRequestFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<AccountRequestFunctionDto> implements AccountRequestFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

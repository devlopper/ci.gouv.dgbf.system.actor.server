package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AccountRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AccountRepresentationImpl extends AbstractRepresentationEntityImpl<AccountDto> implements AccountRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

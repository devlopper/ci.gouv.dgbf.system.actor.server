package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AccountRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AccountRequestRepresentationImpl extends AbstractRepresentationEntityImpl<AccountRequestDto> implements AccountRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

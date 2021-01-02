package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AccountingServiceRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountingServiceDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AccountingServiceRepresentationImpl extends AbstractRepresentationEntityImpl<AccountingServiceDto> implements AccountingServiceRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ExpenditureNatureRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExpenditureNatureDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ExpenditureNatureRepresentationImpl extends AbstractRepresentationEntityImpl<ExpenditureNatureDto> implements ExpenditureNatureRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

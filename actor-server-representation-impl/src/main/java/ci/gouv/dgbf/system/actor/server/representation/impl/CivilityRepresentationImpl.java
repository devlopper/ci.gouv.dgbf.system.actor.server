package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.CivilityRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.CivilityDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class CivilityRepresentationImpl extends AbstractRepresentationEntityImpl<CivilityDto> implements CivilityRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

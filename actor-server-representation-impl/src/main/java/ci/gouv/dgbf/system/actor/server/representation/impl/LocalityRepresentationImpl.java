package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.LocalityRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.LocalityDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class LocalityRepresentationImpl extends AbstractRepresentationEntityImpl<LocalityDto> implements LocalityRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

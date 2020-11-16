package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationAttributRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationAttributDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentificationAttributRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationAttributDto> implements IdentificationAttributRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

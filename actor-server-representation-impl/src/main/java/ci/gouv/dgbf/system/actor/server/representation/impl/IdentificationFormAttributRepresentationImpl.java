package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationFormAttributRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationFormAttributDto> implements IdentificationFormAttributRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

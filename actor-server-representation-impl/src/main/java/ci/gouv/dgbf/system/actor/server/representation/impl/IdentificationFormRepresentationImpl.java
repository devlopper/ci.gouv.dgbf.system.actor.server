package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationFormRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentificationFormRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationFormDto> implements IdentificationFormRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.SectionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.SectionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class SectionRepresentationImpl extends AbstractRepresentationEntityImpl<SectionDto> implements SectionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}

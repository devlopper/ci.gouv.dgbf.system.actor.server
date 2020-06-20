package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ScopeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeFunctionDto> implements ScopeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
